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
    File        : start.p
    Purpose     : Reference Application Startup Procedure for SmartComponent
                  Library / SmartFramework based applications

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 21 14:25:53 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&scoped-define ApplicationTitle Advantzware
&scoped-define BaseRegistryKey Software~\Advantzware~\Advantzware~\
&scoped-define DefaultAppServerPartition Default
&scoped-define ServiceFile Consultingwerk/Windows/Framework/Reference/services.xml
&scoped-define SmartServiceFile Advantzware/Windows/SmartFramework/Menu/smartservices.xml
&scoped-define SmartServiceFileClient Consultingwerk/SmartFramework/services_client.xml
&scoped-define StartupForm Advantzware.Windows.SmartFramework.Menu.MainMenuForm
&scoped-define TaskbarApplicationId com.advantzware.mainmenu
&scoped-define WaitStateIcon Consultingwerk/Framework/NotifyIcons/server_client.ico
&scoped-define WaitStateIconActive Consultingwerk/Framework/NotifyIcons/server_client_exchange.ico

USING Consultingwerk.Framework.*            FROM PROPATH .
USING Consultingwerk.OERA.*                 FROM PROPATH .
USING Consultingwerk.OERA.Enum.*            FROM PROPATH .
USING Consultingwerk.OERA.Context.*         FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.Util.*                 FROM PROPATH .

DEFINE VARIABLE oForm             AS {&StartupForm}                                           NO-UNDO .
DEFINE VARIABLE oCustomizer       AS Infragistics.Shared.ResourceCustomizer                   NO-UNDO .
DEFINE VARIABLE oTaskbarManager   AS Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager      NO-UNDO .
DEFINE VARIABLE cDefaultPartition AS CHARACTER                                                NO-UNDO INIT "{&DefaultAppServerPartition}":U.
DEFINE VARIABLE oParamDictionary  AS Consultingwerk.Framework.Collections.CharacterDictionary NO-UNDO .

DEFINE VARIABLE oTempFileManager  AS ITempFileManager                                         NO-UNDO .

/* ***************************  Main Block  *************************** */

{methods/defines/globdefs.i &NEW=NEW}
{methods/defines/hndldefs.i &NEW=NEW}

/* Mike Fechner, Consultingwerk Ltd. 01.05.2013
   Best default for GUI apps */
SESSION:APPL-ALERT-BOXES = TRUE .
SESSION:DEBUG-ALERT = TRUE .
SESSION:ERROR-STACK-TRACE = TRUE  .
SESSION:SYSTEM-ALERT-BOXES = TRUE .

IF Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:IsPlatformSupported THEN DO:
    oTaskbarManager = Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:Instance .
    oTaskbarManager:ApplicationId = "{&TaskbarApplicationId}":U .
END.

/* Mike Fechner, Consultingwerk Ltd. 20.01.2013
   Integration of the ConfigutaionProvider */
DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .
&IF PROVERSION NE "10.2B" &THEN
/* JSON based config provider only from 11.x */
oConfigurationProvider = {Consultingwerk/get-service.i
                            Consultingwerk.Framework.IConfigurationProvider
                            "NEW Consultingwerk.Framework.ConfigurationProvider ('.applicationsettings':U)"} .
&ELSE
oConfigurationProvider = {Consultingwerk/get-service.i
                            Consultingwerk.Framework.IConfigurationProvider
                            "NEW Consultingwerk.Framework.XmlConfigurationProvider ('.applicationsettings.xml':U)"} .
&ENDIF

/* Mike Fechner, Consultingwerk Ltd. 03.01.2014
   Register ISmtpConfiguration Service when the .applicationsettings file
   contains an Entry for SmptHostName */
IF oConfigurationProvider:GetValue ("SmtpHostName":U) > "":U THEN DO:
   DEFINE VARIABLE oSmtpConfiguration AS SmtpConfiguration NO-UNDO .

   oSmtpConfiguration = NEW SmtpConfiguration () .
   oSmtpConfiguration:SmtpHostName   = oConfigurationProvider:GetValue ("SmtpHostName":U) .
   oSmtpConfiguration:SmtpPassword   = oConfigurationProvider:GetValue ("SmtpPassword":U, "":U) .
   oSmtpConfiguration:SmtpPortNumber = INTEGER (oConfigurationProvider:GetValue ("SmtpPortNumber":U, ?)) .
   oSmtpConfiguration:SmtpSenderName = oConfigurationProvider:GetValue ("SmtpSenderName":U, "":U) .
   oSmtpConfiguration:SmtpUserName   = oConfigurationProvider:GetValue ("SmtpUserName":U, "":U) .

   FrameworkSettings:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.Framework.ISmtpConfiguration":U),
                                                  oSmtpConfiguration) .
END.

/* Mike Fechner, Consultingwerk Ltd. 14.04.2011
   Activate Error output to client logfile */
ErrorHelper:RegisterErrorHandler (NEW LogManagerErrorHandler ()) .

/* Mike Fechner, Consultingwerk Ltd. 14.04.2011
   Enable ServiceInterface Logging (for local data access only) */
ServiceInterface:LoggingLevel = ServiceInterfaceLoggingLevelEnum:Request .
DataAccess:LogFetchDataDetails = TRUE .
LogManager:AddCustomLogEntries ("ServiceAdapter,ServiceInterface,DataAccess,KeepAliveService,ServiceLoader":U) .
/*CAST (FrameworkSettings:ServiceAdapter, ServiceAdapter):LoggingActive = TRUE .*/

/* Mike Fechner, Consultingwerk Ltd. 03.01.2011
   Translate Infragistics Texts */
oCustomizer = Infragistics.Win.Resources:Customizer  .

oCustomizer:SetCustomizedString ("EditContextMenuCopy":U, "&Kopieren"{&TRAN}) .
oCustomizer:SetCustomizedString ("EditContextMenuCut":U, "&Auschneiden"{&TRAN}) .
oCustomizer:SetCustomizedString ("EditContextMenuPaste":U, "&Einfügen"{&TRAN}) .
oCustomizer:SetCustomizedString ("EditContextMenuSelectAll":U, "A&lles Auswählen"{&TRAN}) .

/* Mike Fechner, Consultingwerk Ltd. 22.06.2010
   Use AppServerServiceManager when sports2000 DB is not connected
   See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Managing+AppServer+Connections
   for information about how to manage AppServer connections */
IF NUM-DBS = 0 OR ListHelper:CanDo (SESSION:PARAM, "AppServerConnect":U) THEN DO:

    /* Mike Fechner, Consultingwerk Ltd. 03.11.2013
       Default Partition from -param? */
    oParamDictionary = ListHelper:AlternatingListToDictionary (SESSION:PARAM, ",":U, "=":U) .

    /* Mike Fechner, Consultingwerk Ltd. 31.08.2015
       SCL-994: AppServerServiceManager based on -param AppServer=... */
    IF oParamDictionary:ContainsKey ("AppServer":U) THEN DO:
        FrameworkSettings:AppServerServiceManager = NEW StartupParameterAppServerServiceManager (oParamDictionary) .

        ASSIGN cDefaultPartition = "server":U .
    END.
    ELSE DO:
        FrameworkSettings:AppServerServiceManager = NEW AppServerServiceManager () .

        IF oParamDictionary:ContainsKey ("defaultPartition":U) THEN
            cDefaultPartition = oParamDictionary:GetValue ("defaultPartition":U) .
        ELSE IF VALID-OBJECT (oConfigurationProvider) THEN
            cDefaultPartition = oConfigurationProvider:GetValue ("defaultPartition":U, cDefaultPartition) .
        ELSE
            cDefaultPartition = cDefaultPartition .

        /* Mike Fechner, Consultingwerk Ltd. 20.07.2011
           New way of defining a default partition for AppServer calls */
        CAST (FrameworkSettings:AppServerServiceManager,
              AppServerServiceManager):DefaultPartition = cDefaultPartition .
    END.

    /* Mike Fechner, Consultingwerk Ltd. 18.04.2013
       Attempt to connect to the AppServer during startup */
    DO ON ERROR UNDO, THROW:
        FrameworkSettings:AppServerServiceManager:ConnectService (cDefaultPartition) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .

            QUIT .
        END CATCH .
    END .

    FrameworkSettings:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.Framework.RepositoryServices.IRepositoryService":U),
                                                   NEW Consultingwerk.Framework.RepositoryServices.RepositoryServiceClient ()) .

    /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
       When connecting to the AppServer, we need the context serializer */
    FrameworkSettings:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.Framework.Server.SessionContextSerializer":U),
                                                   NEW Consultingwerk.Framework.Server.SessionContextSerializer ()) .
END.

Consultingwerk.Framework.FrameworkSettings:ApplicationLabel = "{&ApplicationTitle}"{&TRAN} .
Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey = "{&BaseRegistryKey}":U .
Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE .
Consultingwerk.Framework.FrameworkSettings:StoreRibbonQuickAccessToolbar = TRUE .
Consultingwerk.Framework.FrameworkSettings:StoreUltraSplitterPosition = TRUE .
&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
Consultingwerk.SmartComponents.Implementation.SmartDataBrowser:SaveColumnSettings = TRUE .
&ELSE
Consultingwerk.SmartComponents.Implementation.SmartDataBrowserSettings:SaveColumnSettings = TRUE .
&ENDIF

/* Mike Fechner, Consultingwerk Ltd. 23.05.2014
   Close Forms automatically, when "Exit" is clicked */
Consultingwerk.SmartComponents.Implementation.SmartToolbarControllerSettings:SmartToolbarControllerDefaultExit = TRUE .

/* Mike Fechner, Consultingwerk Ltd. 07.11.2011
   Load services using ServiceLoader */
DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO .

oLoader = NEW ServiceLoader () .
oLoader:Load ("{&ServiceFile}":U) .

/* Mike Fechner, Consultingwerk Ltd. 19.10.2011
   Initialize Session Context Dataset */
DEFINE VARIABLE oContextDatasetFactory AS IContextDatasetFactory NO-UNDO .
oContextDatasetFactory = {Consultingwerk/get-service.i Consultingwerk.OERA.Context.IContextDatasetFactory} .
Consultingwerk.Framework.Session.SessionManager:ContextDataset = oContextDatasetFactory:CreateContextDataset() .

&IF DEFINED (SmartFramework) NE 0 &THEN
oLoader:Load ("{&SmartServiceFile}":U) .
Consultingwerk.OERA.ServiceInterface:EvaluateRequestAuthorizationProvider () .

IF NOT CONNECTED ("SmartDB":U) THEN
    oLoader:Load ("{&SmartServiceFileClient}":U) .

&ENDIF

oLoader:Load ("Advantzware/WinKit/services.xml":U) .

DELETE OBJECT oLoader .

/*/* Mike Fechner, Consultingwerk Ltd. 04.03.2012                                              */
/*   Perform Infragistics UI Translation */                                                    */
/*DEFINE VARIABLE oLocalizer AS Consultingwerk.Windows.Localization.ILocalizer NO-UNDO .       */
/*oLocalizer = NEW Consultingwerk.SmartComponentsDemo.Windows.Localization.Infragistics.DE () .*/
/*oLocalizer:Localize() .                                                                      */
/*oLocalizer = ? . /* Let the GC do it's work when the ILocalizer is no longer required */     */

Consultingwerk.Util.ClassHelper:UseExternalRuntimeForGetClassNames = TRUE .

oForm = NEW {&StartupForm} () .

/* Mike Fechner, Consultingwerk Ltd. 05.09.2011
   Customize WaitStateManager Icons */
FrameworkSettings:WaitStateManager = NEW NotifyIconWaitStateManager
                                        ("{&WaitStateIcon}":U,
                                         "{&WaitStateIconActive}":U) .

/* Mike Fechner, Consultingwerk Ltd. 04.07.2015
   Map Enter to Tab */
SmartWindowFormController:MapEnterKeyToTab = TRUE .

Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE .
Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive = TRUE .
Consultingwerk.WindowIntegrationKit.Forms.WinKitForms:TabFolderBGColor = 31 .

WAIT-FOR System.Windows.Forms.Application:Run (oForm) .

ASSIGN oTempFileManager = {Consultingwerk/get-service.i Consultingwerk.Framework.ITempFileManager} .

IF VALID-OBJECT (oTempFileManager) THEN
    oTempFileManager:CleanUp() .

DELETE OBJECT Consultingwerk.Framework.FrameworkSettings:WaitStateManager .

/* Mike Fechner, Consultingwerk Ltd. 01.04.2013
   Handle typical .NET Exception, indicating that an Assembly is not available */
CATCH fioex AS System.IO.FileNotFoundException:
    DEFINE VARIABLE cStack AS CHARACTER NO-UNDO.

    IF SESSION:ERROR-STACK-TRACE = TRUE THEN DO:
        cStack = ENTRY (NUM-ENTRIES (fioex:CallStack, CHR(10)), fioex:CallStack, CHR(10)) .

        IF ENTRY (1, cStack, " ":U) = "InitializeComponent":U THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage
                (SUBSTITUTE ("Error loading .NET Assembly:&1&1&2"{&TRAN},
                             System.Environment:NewLine,
                             fioex:FileName),
                 "{&ApplicationTitle}"{&TRAN},
                 Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .

            LEAVE .
        END.
    END.

    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (fioex) .
END CATCH.

CATCH e AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .
END CATCH.
