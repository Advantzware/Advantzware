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
    File        : SessionManager
    Purpose     : Static class that manages session and authentication
                  properties
    Syntax      :
    Description : The method SessionManager:LogSessionManagerProperties
                  requires the "SessionManager" custom LogEntryType activated
                  within the LogManager
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 15 22:47:24 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.Framework.Exceptions.*          FROM PROPATH .
USING Consultingwerk.Framework.Session.*             FROM PROPATH .
USING Consultingwerk.SmartFramework.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.Framework.Session.SessionManager:

    {Consultingwerk/SmartFramework/ttLoginCompany.i &ACCESS=STATIC &NO-BEFORE}

    /*------------------------------------------------------------------------------
        Purpose: Raised when the DisplayLanguage Changes
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT LanguageDisplayNameChanged SIGNATURE VOID ().

    /*------------------------------------------------------------------------------
        Purpose: Raised when the Language Iso Code Changes
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT LanguageIsoCodeChanged SIGNATURE VOID ().

    /*------------------------------------------------------------------------------
        Purpose: Raised when the Language Key Changes
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT LanguageKeyChanged SIGNATURE VOID ().

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyKey has changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT LoginCompanyKeyChanged SIGNATURE VOID ().

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyName has changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT LoginCompanyNameChanged SIGNATURE VOID ().

    /*------------------------------------------------------------------------------
        Purpose: Raised when the Login has completed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT LoginCompleted SIGNATURE VOID ().

    /*------------------------------------------------------------------------------
        Purpose: Raised when the UserName has changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT UserNameChanged SIGNATURE VOID ().

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the RAW export of the client-principal
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ClientPrincipal AS RAW NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets the reference to the current context dataset
        Notes:   On the backend this dataset is only expected to exist during a
                 client request. This property is implemented to support a custom
                 context management strategy. A ProDataset can be shared between
                 frontend and backend to carry custom context information.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ContextDataset AS HANDLE NO-UNDO
    GET.
    SET (INPUT arg AS HANDLE):
        IF VALID-HANDLE (arg) THEN
            {Consultingwerk/Assertion/HandleAssert/WidgetType.i arg ""DATASET"":U} .

        SessionManager:ContextDataset = arg .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Current Language DisplayName
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LanguageDisplayName AS CHARACTER NO-UNDO
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO.

        ASSIGN lChanged = (SessionManager:LanguageDisplayName <> arg) .

        SessionManager:LanguageDisplayName = arg .

        IF lChanged THEN
            SessionManager:OnLanguageDisplayNameChanged() .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Current Language Iso Code
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LanguageIsoCode AS CHARACTER NO-UNDO
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO.

        ASSIGN lChanged = (SessionManager:LanguageIsoCode <> arg) .

        SessionManager:LanguageIsoCode = arg .

        IF lChanged THEN
            SessionManager:OnLanguageIsoCodeChanged() .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Current Language Key
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LanguageKey AS CHARACTER NO-UNDO
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO.

        ASSIGN lChanged = (SessionManager:LanguageKey <> arg) .

        SessionManager:LanguageKey = arg .

        IF lChanged THEN
            SessionManager:OnLanguageKeyChanged() .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Key of the current login company
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LoginCompanyKey AS CHARACTER NO-UNDO
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO.

        ASSIGN lChanged = (SessionManager:LoginCompanyKey <> arg) .

        SessionManager:LoginCompanyKey = arg .

        IF lChanged THEN
            SessionManager:OnLoginCompanyKeyChanged() .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the current login company
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LoginCompanyName AS CHARACTER NO-UNDO
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO.

        ASSIGN lChanged = (SessionManager:LoginCompanyName <> arg) .

        SessionManager:LoginCompanyName = arg .

        IF lChanged THEN
            SessionManager:OnLoginCompanyNameChanged() .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the short name of the current login company
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LoginCompanyShortName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the key of the large login company image
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LoginCompanyImageLarge AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the key of the small login company image
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LoginCompanyImageSmall AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the foreign reference character value of the current login company
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LoginCompanyReferenceChar AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the foreign reference character value of the current login company
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LoginCompanyReferenceDecimal AS DECIMAL NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the foreign reference character value of the current login company
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LoginCompanyReferenceInteger AS INTEGER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the class name of the LoginForm that should be used
        Notes:   The LoginForm should implement the Consultingwerk.Framework.Session.ILoginForm
                 Interface
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LoginFormClassName AS CHARACTER NO-UNDO
       INITIAL "Consultingwerk.Windows.Session.LoginForm":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Allows the client to specify the FrameworkSettings:DebugMode for
                 the AppServer
        Notes:   Typically set on the client
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ServerDebugMode AS LOGICAL NO-UNDO INITIAL ?
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Allows the client to specify the LogManager:CustomLogEntries for
                 the AppServer
        Notes:   Typically set on the client
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ServerCustomLogEntries AS CHARACTER NO-UNDO INITIAL ?
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Allows the client to specify the LOG-MANAGER:LOG-ENTRY-TYPES for
                 the AppServer
        Notes:   Typically set on the client
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ServerLogEntryTypes AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the keys of the UserGroups
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UserGroupKeys AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the names of the UserGroups
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UserGroupNames AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current user's key value (e.g. SmartUser.UserGuid)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UserKey AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current user name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UserName AS CHARACTER NO-UNDO
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO.

        ASSIGN lChanged = (SessionManager:UserName <> arg) .

        SessionManager:UserName = arg .

        IF lChanged THEN
            SessionManager:OnUserNameChanged() .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose:  Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE SessionManager ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs the application login
        Notes:   Actual Login routine including
        @param poUserCredentials The IUserCredentials with the data for this method
        @return Logical value indicating the success of the operation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL Login (poUserCredentials AS IUserCredentials):

        DEFINE VARIABLE oAuthenticationService AS IAuthenticationService           NO-UNDO .
        DEFINE VARIABLE oLoginCompanyProvider  AS ILoginCompanyProvider            NO-UNDO .
        DEFINE VARIABLE hDataset               AS HANDLE                           NO-UNDO .
        &IF DEFINED (SmartFramework) NE 0 &THEN
        DEFINE VARIABLE oGetGroupsParameter    AS GetUserGroupAssignmentsParameter NO-UNDO .
        &ENDIF

        oAuthenticationService = {Consultingwerk/get-service.i Consultingwerk.Framework.IAuthenticationService} .
        oLoginCompanyProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ILoginCompanyProvider} .

        IF VALID-OBJECT (oLoginCompanyProvider) THEN
            oLoginCompanyProvider:GetAvailableLoginCompanies (OUTPUT TABLE ttLoginCompany) .

        /* Mike Fechner, Consultingwerk Ltd. 18.01.2013
           When we are not using the SmartFramework, we may well proceed even without
           an Authentication Service (optional for demo purposes only) */
        &IF DEFINED (SmartFramework) NE 0 &THEN
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oAuthenticationService
                                                         """Authentication Service"":U"} .
        &ENDIF

        IF VALID-OBJECT (oAuthenticationService) THEN DO:
            IF NOT oAuthenticationService:ValidateUser (poUserCredentials) THEN DO:
                UNDO, THROW NEW AuthenticationException ("Unable to login!"{&TRAN}, 0) .
            END.
        END.

        SessionManager:UserName = poUserCredentials:UserName .
        SessionManager:LoginCompanyKey  = poUserCredentials:CompanyKey .
        SessionManager:LoginCompanyName = poUserCredentials:CompanyName .

        IF VALID-OBJECT (oLoginCompanyProvider) THEN DO:
            FIND ttLoginCompany WHERE ttLoginCompany.LoginCompanyKey = poUserCredentials:CompanyKey NO-ERROR .

            IF AVAILABLE ttLoginCompany THEN DO:
                ASSIGN
                    SessionManager:LoginCompanyKey              = ttLoginCompany.LoginCompanyKey
                    SessionManager:LoginCompanyName             = ttLoginCompany.LoginCompanyName
                    SessionManager:LoginCompanyShortName        = ttLoginCompany.LoginCompanyShort
                    SessionManager:LoginCompanyImageLarge       = ttLoginCompany.LoginCompanyImageLarge
                    SessionManager:LoginCompanyImageSmall       = ttLoginCompany.LoginCompanyImageSmall
                    SessionManager:LoginCompanyReferenceChar    = ttLoginCompany.LoginCompanyReferenceChar
                    SessionManager:LoginCompanyReferenceDecimal = ttLoginCompany.LoginCompanyReferenceDecimal
                    SessionManager:LoginCompanyReferenceInteger = ttLoginCompany.LoginCompanyReferenceInteger
                .
            END.
        END.

        /* User Groups */
        &IF DEFINED (SmartFramework) NE 0 &THEN
        oGetGroupsParameter = NEW GetUserGroupAssignmentsParameter (SessionManager:UserName,
                                                                    SessionManager:LoginCompanyKey) .

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                                       "GetUserGroupAssignments":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                       oGetGroupsParameter) .

        ASSIGN
            SessionManager:UserGroupKeys  = oGetGroupsParameter:GroupGuids
            SessionManager:UserGroupNames = oGetGroupsParameter:GroupNames .
        &ENDIF

        RETURN TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the application login
        Notes:   Shows the Login Dialog
        @param plOk OUTPUT Logical value indicating if login was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Login (OUTPUT plOk AS LOGICAL):

        &IF DEFINED (DotNetAccessible) NE 0 &THEN
        Login (Consultingwerk.Framework.FrameworkSettings:MdiContainer, OUTPUT plOk) .
        &ELSE
        UNDO, THROW NEW AppError ("This method is not supported on the current platform", 0) .
        &ENDIF

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Performs the application login
        Notes:   Shows the Login Dialog
        @param poParent The parent Form for the Login Dialog
        @param lOk OUTPUT Logical value indicating if login was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Login (poParent AS System.Windows.Forms.Form,
                                     OUTPUT lOk AS LOGICAL):

        DEFINE VARIABLE oDialogResult          AS System.Windows.Forms.DialogResult           NO-UNDO .
        DEFINE VARIABLE oLoginForm             AS Consultingwerk.Framework.Session.ILoginForm NO-UNDO .
        DEFINE VARIABLE oForm                  AS System.Windows.Forms.Form                   NO-UNDO .
        DEFINE VARIABLE lBpmLoginOk            AS LOGICAL                                     NO-UNDO .
        DEFINE VARIABLE oCredentialProvider    AS ICredentialProvider                         NO-UNDO .
        DEFINE VARIABLE oCredentials           AS IUserCredentials                            NO-UNDO .
        DEFINE VARIABLE oAuthenticationService AS IAuthenticationService                      NO-UNDO .
        DEFINE VARIABLE lAuthenticated         AS LOGICAL INITIAL FALSE                       NO-UNDO .

        &IF DEFINED (OpenEdgeBPM) NE 0 &THEN
        DEFINE VARIABLE oBpmAdapter AS Consultingwerk.BPM.IBPMAdapterProxy NO-UNDO .
        oBpmAdapter = { Consultingwerk/get-service.i Consultingwerk.BPM.IBPMAdapterProxy
                                                     "NEW Consultingwerk.BPM.BPMAdapterProxy ()"} .
        &ENDIF

        oCredentialProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.Session.ICredentialProvider} .

        IF VALID-OBJECT (oCredentialProvider) THEN DO:
            oCredentials = oCredentialProvider:GetUserCredentials () .

            IF VALID-OBJECT (oCredentials) THEN DO:
                oAuthenticationService = {Consultingwerk/get-service.i Consultingwerk.Framework.IAuthenticationService} .

                IF VALID-OBJECT (oAuthenticationService) THEN DO:
                    lAuthenticated = oAuthenticationService:ValidateUser (oCredentials) .

                    IF lAuthenticated THEN
                        ASSIGN lOk = TRUE .

                    &IF DEFINED (OpenEdgeBPM) NE 0 &THEN
                    oBpmAdapter:StoreBpmCredentials (oCredentials:UserName,
                                                     oCredentials:Password) .

                    Consultingwerk.OERA.ContextWrapper:SetLogicalContextProperty ("BpmSignOnToBpm":U,
                                                                                  TRUE) .
                    &ENDIF
                END.
            END.
        END.

        IF NOT lAuthenticated THEN DO:
            oLoginForm = DYNAMIC-NEW (SessionManager:LoginFormClassName) () .

            oForm = CAST (CAST (oLoginForm, Progress.Lang.Object),
                          System.Windows.Forms.Form) .

            IF VALID-OBJECT (poParent) THEN DO:
                oForm:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent .

                WAIT-FOR oForm:ShowDialog (poParent) SET oDialogResult .
            END.
            ELSE
                WAIT-FOR oForm:ShowDialog () SET oDialogResult .

            IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:Ok) THEN
                ASSIGN lOk = TRUE .
            ELSE
                ASSIGN lOk = FALSE .
        END.

        &IF DEFINED (OpenEdgeBPM) NE 0 &THEN
        UserInterfaceHelper:ProcessEvents() .

        IF ContextWrapper:GetLogicalContextProperty ("BpmSignOnToBpm":U) THEN DO:

            ASSIGN lBpmLoginOk = FALSE .

            DO ON ERROR UNDO, THROW:
                /* Attempt to perform single sign on to BPM server */
                ASSIGN lBpmLoginOk = oBpmAdapter:LoginWithStoredCredentials () .

                CATCH err AS Progress.Lang.Error:
                    /* Mike Fechner, Consultingwerk Ltd. 07.11.2012
                       Ignore any error from the Login attempt with the stored credentials.
                       In this case, attempt to login usnig the Login Form */
                    /*ErrorHelper:ShowErrorMessage (err) .*/
                END CATCH.
            END.

            DO WHILE NOT lBpmLoginOk ON ERROR UNDO, THROW:

                lBpmLoginOk = oBpmAdapter:Login (poParent) .

                LEAVE .

                CATCH err AS Progress.Lang.Error:
                    ErrorHelper:ShowErrorMessage (err) .
                END CATCH.
            END.
        END.
        &ENDIF

        IF lOk THEN
            OnLoginCompleted () .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oLoginForm) .
        END FINALLY.

    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Writes the current SessionManager properties to the log file
        Notes:   The method requires the "SessionManager" custom LogEntryType activated
                 within the LogManager
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LogSessionManagerProperties ():

        IF LOOKUP ("SessionManager":U, LogManager:CustomLogEntries) = 0 THEN
            RETURN .

        LogManager:WriteFormattedMessage ("[SessionManager]: LanguageDisplayName          = &1":U, SessionManager:LanguageDisplayName) .
        LogManager:WriteFormattedMessage ("[SessionManager]: LanguageIsoCode              = &1":U, SessionManager:LanguageIsoCode) .
        LogManager:WriteFormattedMessage ("[SessionManager]: LanguageKey                  = &1":U, SessionManager:LanguageKey) .
        LogManager:WriteFormattedMessage ("[SessionManager]: LoginCompanyKey              = &1":U, SessionManager:LoginCompanyKey) .
        LogManager:WriteFormattedMessage ("[SessionManager]: LoginCompanyName             = &1":U, SessionManager:LoginCompanyName) .
        LogManager:WriteFormattedMessage ("[SessionManager]: LoginCompanyReferenceChar    = &1":U, SessionManager:LoginCompanyReferenceChar) .
        LogManager:WriteFormattedMessage ("[SessionManager]: LoginCompanyReferenceDecimal = &1":U, STRING (SessionManager:LoginCompanyReferenceDecimal)) .
        LogManager:WriteFormattedMessage ("[SessionManager]: LoginCompanyReferenceInteger = &1":U, STRING (SessionManager:LoginCompanyReferenceInteger)) .
        LogManager:WriteFormattedMessage ("[SessionManager]: LoginCompanyShortName        = &1":U, SessionManager:LoginCompanyShortName) .
        LogManager:WriteFormattedMessage ("[SessionManager]: UserName                     = &1":U, SessionManager:UserName) .
        LogManager:WriteFormattedMessage ("[SessionManager]: UserGroupKeys                = &1":U, SessionManager:UserGroupKeys) .
        LogManager:WriteFormattedMessage ("[SessionManager]: UserGroupNames               = &1":U, SessionManager:UserGroupNames) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LanguageDisplayNameChanged event
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnLanguageDisplayNameChanged ():

        SessionManager:LanguageDisplayNameChanged:Publish () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LanguageDisplayNameChanged event
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnLanguageIsoCodeChanged ():

        SessionManager:LanguageIsoCodeChanged:Publish () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LanguageDisplayNameChanged event
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnLanguageKeyChanged ():

        SessionManager:LanguageKeyChanged:Publish () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyKeyChanged event
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnLoginCompanyKeyChanged ():

        SessionManager:LoginCompanyKeyChanged:Publish () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyNameChanged event
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnLoginCompanyNameChanged ():

        SessionManager:LoginCompanyNameChanged:Publish () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompleted event
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnLoginCompleted ():

        SessionManager:LoginCompleted:Publish () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the UserNameChanged event
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnUserNameChanged ():

        SessionManager:UserNameChanged:Publish () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Resets all Session Manager Properties to their initial values
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Reset ():

        IF VALID-HANDLE (SessionManager:ContextDataset) AND SessionManager:ContextDataset:DYNAMIC THEN
            DELETE OBJECT SessionManager:ContextDataset .

        ASSIGN SessionManager:ClientPrincipal              = ?
               SessionManager:ContextDataset               = ?
               SessionManager:LanguageDisplayName          = "":U
               SessionManager:LanguageIsoCode              = "":U
               SessionManager:LanguageKey                  = "":U
               SessionManager:LoginCompanyImageLarge       = "":U
               SessionManager:LoginCompanyImageSmall       = "":U
               SessionManager:LoginCompanyKey              = "":U
               SessionManager:LoginCompanyName             = "":U
               SessionManager:LoginCompanyReferenceChar    = "":U
               SessionManager:LoginCompanyReferenceDecimal = 0.0
               SessionManager:LoginCompanyReferenceInteger = 0
               SessionManager:LoginCompanyShortName        = "":U
               SessionManager:ServerCustomLogEntries       = ?
               SessionManager:ServerDebugMode              = ?
               SessionManager:ServerLogEntryTypes          = "":U
               SessionManager:UserGroupKeys                = "":U
               SessionManager:UserGroupNames               = "":U
               SessionManager:UserName                     = "":U
               SessionManager:UserKey                      = "":U.

    END METHOD .

END CLASS.
