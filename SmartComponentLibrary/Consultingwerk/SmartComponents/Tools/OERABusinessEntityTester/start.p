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
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 16 19:07:13 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*    FROM PROPATH .
USING Consultingwerk.OERA.*         FROM PROPATH .
USING Consultingwerk.OERA.Enum.*    FROM PROPATH .
USING Consultingwerk.OERA.Context.* FROM PROPATH .  
USING Consultingwerk.Util.*         FROM PROPATH .

DEFINE VARIABLE oForm AS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.UltraBusinessEntityTesterForm NO-UNDO . 

DEFINE VARIABLE oTaskbarManager AS Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager NO-UNDO . 

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 01.05.2013
   Best default for GUI apps */
SESSION:APPL-ALERT-BOXES = TRUE . 
SESSION:DEBUG-ALERT = TRUE . 
SESSION:ERROR-STACK-TRACE = TRUE  .
SESSION:SYSTEM-ALERT-BOXES = TRUE . 

IF Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:IsPlatformSupported THEN DO:
    oTaskbarManager = Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:Instance . 
    oTaskbarManager:ApplicationId = "de.consultingwerk.businessentitytester":U . 
END.

IF PROVERSION BEGINS "10.":U THEN 
    FILE-INFO:FILE-NAME = "Consultingwerk\Windows\Styles\Office2007Black_ConsultingwerkStudio.isl":U .
ELSE     
    FILE-INFO:FILE-NAME = "Consultingwerk\Windows\Styles\Office2010Blue_ConsultingwerkStudio.isl":U .

IF FILE-INFO:FULL-PATHNAME > "":U THEN 
    Consultingwerk.Util.StyleLibraryHelper:LoadFromFile (FILE-INFO:FULL-PATHNAME) .

/* Mike Fechner, Consultingwerk Ltd. 20.01.2013
   Perform Database Authentication if required */
RUN Consultingwerk/Windows/OpenEdgeLogin/_prostar.p . 

/* Mike Fechner, Consultingwerk Ltd. 14.04.2011
   Activate Error output to client logfile */
ErrorHelper:RegisterErrorHandler (NEW LogManagerErrorHandler ()) .

/* Mike Fechner, Consultingwerk Ltd. 14.04.2011
   Enable ServiceInterface Logging */
ServiceInterface:LoggingLevel = ServiceInterfaceLoggingLevelEnum:Request .
DataAccess:LogFetchDataDetails = TRUE . 
LogManager:CustomLogEntries = "ServiceInterface,DataAccess":U .

/* Mike Fechner, Consultingwerk Ltd. 27.12.2013
   Enable tracing of Query Info on client */
{Consultingwerk/get-service.i Consultingwerk.OERA.IDataSourceQueryInfoProvider
                              "NEW Consultingwerk.OERA.DataSourceQueryInfoProvider()"} .

Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey = "Software~\Consultingwerk Ltd.~\Business Entity Designer~\":U .
Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE . 
&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
Consultingwerk.SmartComponents.Implementation.SmartDataBrowser:SaveColumnSettings = TRUE . 
&ELSE 
Consultingwerk.SmartComponents.Implementation.SmartDataBrowserSettings:SaveColumnSettings = TRUE . 
&ENDIF

oForm = NEW Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.UltraBusinessEntityTesterForm () . 

WAIT-FOR System.Windows.Forms.Application:RUN (oForm) .

DELETE OBJECT Consultingwerk.Framework.FrameworkSettings:WaitStateManager .

QUIT . 

CATCH e AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .      
END CATCH.
