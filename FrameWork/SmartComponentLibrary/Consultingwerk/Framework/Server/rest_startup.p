/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : rest_startup.p
    Purpose     : AppServer Startup Procedure for REST AppServer

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 26 23:55:01 CET 2012
    Notes       : Supported Parameters for startup procedure (comma delimited list):
                  DebugMode:           Turn on FrameworkSettings:DebugMode for
                                       various logging/tracing options
                  OeraSiOnly:          Use SESSION:EXPORT to only allow access to
                                       Service Interface procedures in AppServer calls
                  CustomLogEntries:    Pipe-delimited list of CustomLogEntries to be
                                       activated. Note, that when DebugMode is set a
                                       default list of log entries will be activated
                                       when this parameter is not set, sample
                                       CustomLogEntries=ServiceInterface|DataAccess|ServiceLoader|ConfigurationProvider
                  Services:            Pipe-delimited list of services.xml files to the
                                       loaded
                  LoadDefaultServices: true/yes or false/no, allows to control if the
                                       default service.xml files should be loaded (default
                                       is true)
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.Exceptions.*            FROM PROPATH .
USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .

{Consultingwerk/products.i}

DEFINE INPUT  PARAMETER pcParameter AS CHARACTER NO-UNDO.

DEFINE VARIABLE oValidationService     AS IStartupValidationService NO-UNDO .

DEFINE VARIABLE oParamDictionary       AS CharacterDictionary       NO-UNDO .
DEFINE VARIABLE cServices              AS CHARACTER                 NO-UNDO .
DEFINE VARIABLE lLoadDefaultServices   AS LOGICAL                   NO-UNDO INIT TRUE .
DEFINE VARIABLE i                      AS INTEGER                   NO-UNDO .

/* ***************************  Main Block  *************************** */

{Consultingwerk/agentstart-log.i}

oParamDictionary = ListHelper:AlternatingListToDictionary (pcParameter, ",":U, "=":U) .

IF oParamDictionary:ContainsKey ("DebugMode":U) THEN DO:
    FrameworkSettings:DebugMode = TRUE .
    DataAccess:LogFetchDataDetails = TRUE .

    IF NOT oParamDictionary:ContainsKey ("CustomLogEntries":U) THEN
        LogManager:AddCustomLogEntries ("ServiceInterface,DataAccess,ServiceLoader,ConfigurationProvider,ServiceNameMappingService":U) .
END.

/* Only support the Service Interface path to the SmartComponent Library client */
IF oParamDictionary:ContainsKey ("OeraSiOnly":U) THEN
    SESSION:EXPORT ("{&OERASI}/*.*":U) .

IF oParamDictionary:ContainsKey ("CustomLogEntries":U) THEN
    LogManager:AddCustomLogEntries (REPLACE (oParamDictionary:GetValue ("CustomLogEntries":U), "|":U, ",":U)) .

/* Mike Fechner, Consultingwerk Ltd. 20.01.2013
   Integration of the ConfigutaionProvider */
&IF PROVERSION NE "10.2B" &THEN
/* Json based config provider only from 11.x */
/*oConfigurationProvider =*/ {Consultingwerk/get-service.i
                                 Consultingwerk.Framework.IConfigurationProvider
                                 "NEW Consultingwerk.Framework.ConfigurationProvider (FileHelper:FindFile ('.restapplicationsettings':U))"} .
&ELSE
/* Mike Fechner, Consultingwerk Ltd. 28.08.2013
   Xml based configuration provider for OE 10.2B */
/*oConfigurationProvider =*/ {Consultingwerk/get-service.i
                                 Consultingwerk.Framework.IConfigurationProvider
                                 "NEW Consultingwerk.Framework.XmlConfigurationProvider ('.applicationsettings.xml':U)"} .
&ENDIF

/* Mike Fechner, Consultingwerk Ltd. 26.01.2012
   Load services using ServiceLoader */
DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO .

/* Mike Fechner, Consultingwerk Ltd. 14.12.2016
   SCL-1597: Ability to control if default services should be loaded */
IF oParamDictionary:ContainsKey("LoadDefaultServices":U) THEN
    ASSIGN lLoadDefaultServices = DataTypeHelper:ToLogical(oParamDictionary:GetValue("LoadDefaultServices":U)) .

oLoader = NEW ServiceLoader () .

IF lLoadDefaultServices THEN DO:
    oLoader:Load ("Consultingwerk/Framework/Server/rest_services.xml":U) .

    &IF DEFINED (SmartFramework) NE 0 &THEN
    oLoader:Load ("Consultingwerk/SmartFramework/services_server.xml":U) .
    &ENDIF
END.

/* Mike Fechner, Consultingwerk Ltd. 08.09.2016
   Load services from services= parameter */
IF oParamDictionary:ContainsKey ("services":U) THEN DO:
    ASSIGN cServices = oParamDictionary:GetValue ("services":U) .

    DO i = 1 TO NUM-ENTRIES (cServices, "|":U):
        oLoader:Load (ENTRY (i, cServices, "|":U)) .
    END.
END.

DELETE OBJECT oLoader .

/* Ensure the IRequestAuthorizationProvider service is registered with the
   Service Interface */
Consultingwerk.OERA.ServiceInterface:EvaluateRequestAuthorizationProvider () .

IF FrameworkSettings:DebugMode THEN DO:
    LogManager:WriteSeparator () .
    LogManager:WritePropath () .
    LogManager:WriteSeparator () .
END .

/* SCL-1307 - Startup Validation Service */
ASSIGN oValidationService = {Consultingwerk/get-service.i Consultingwerk.Framework.IStartupValidationService} .

IF VALID-OBJECT (oValidationService) THEN
    oValidationService:ValidateSessionStartup() .

LogManager:WriteMessage ("**********************************************************":U) .

/* SCL-1504 - Trial Copy */
&IF DEFINED (TrialVersionMode) NE 0 &THEN
LogManager:WriteMessage ("*** THIS IS A TRIAL COPY OF THE SmartComponent Library ***":U) .
LogManager:WriteMessage ("*** (c)2016 Consultingwerk Ltd.                        ***":U) .
LogManager:WriteMessage ("**********************************************************":U) .
PAUSE 15 .
&ENDIF
LogManager:WriteMessage ("*** AppServer Startup Procedure finished.":U) .
LogManager:WriteMessage ("**********************************************************":U) .

CATCH err AS Progress.Lang.Error:
    LogManager:WriteSeparator() .

    IF TYPE-OF (err, ISupportsInnerException) AND
       VALID-OBJECT (CAST (err, ISupportsInnerException):InnerException) THEN
        LogManager:WriteMessage (ErrorHelper:FormattedErrorMessagesExt (err)) .
    ELSE
        Consultingwerk.Util.LogManager:WriteError (err) .

    LogManager:WriteSeparator() .

    QUIT .

END CATCH.
