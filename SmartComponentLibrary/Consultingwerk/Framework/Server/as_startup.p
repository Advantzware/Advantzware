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
    File        : ab_startup.p
    Purpose     : AppServer Startup Procedure for classic AppServer agent
                  and PASOE session

    Syntax      :  

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 26 23:55:01 CET 2012
    Notes       : Supported Parameters for startup procedure (comma delimited list):
                  DebugMode:        Turn on FrameworkSettings:DebugMode for 
                                    various logging/tracing options
                  OeraSiOnly:       Use SESSION:EXPORT to only allow access to 
                                    Service Interface procedures in AppServer calls
                  CustomLogEntries: Pipe-delimited list of CustomLogEntries to be
                                    activated. Note, that when DebugMode is set a 
                                    default list of log entries will be activated
                                    when this parameter is not set, sample
                                    CustomLogEntries=ServiceInterface|DataAccess|ServiceLoader|ConfigurationProvider
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .

{Consultingwerk/products.i}

DEFINE INPUT  PARAMETER pcParameter AS CHARACTER NO-UNDO.

DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .

DEFINE VARIABLE oParamDictionary       AS CharacterDictionary    NO-UNDO . 

/* ***************************  Main Block  *************************** */

{Consultingwerk/agentstart-log.i}

oParamDictionary = ListHelper:AlternatingListToDictionary (pcParameter, ",":U, "=":U) . 


IF oParamDictionary:ContainsKey ("DebugMode":U) THEN DO: 
    FrameworkSettings:DebugMode = TRUE .
    DataAccess:LogFetchDataDetails = TRUE . 
    
    IF NOT oParamDictionary:ContainsKey ("CustomLogEntries":U) THEN 
        LogManager:AddCustomLogEntries ("ServiceInterface,DataAccess,ServiceLoader,ConfigurationProvider":U) .
END.    
    
/* Only support the Service Interface path to the SmartComponent Library client */
IF oParamDictionary:ContainsKey ("OeraSiOnly":U) THEN
    SESSION:EXPORT ("{&OERASI}/*.*":U) .    
      
IF oParamDictionary:ContainsKey ("CustomLogEntries":U) THEN 
    LogManager:AddCustomLogEntries (REPLACE (oParamDictionary:GetValue ("CustomLogEntries":U), "|":U, ",":U)) .
      
/* Mike Fechner, Consultingwerk Ltd. 20.01.2013
   Integration of the ConfigutaionProvider */
&IF PROVERSION NE "10.2B" &THEN    
/* Jsonbased config provider only from 11.x */
oConfigurationProvider = {Consultingwerk/get-service.i 
                            Consultingwerk.Framework.IConfigurationProvider 
                            "NEW Consultingwerk.Framework.ConfigurationProvider ('.applicationsettings':U)"} .  
&ELSE
/* Mike Fechner, Consultingwerk Ltd. 28.08.2013
   Xml base configuration provider for OE 10.2B */
oConfigurationProvider = {Consultingwerk/get-service.i 
                            Consultingwerk.Framework.IConfigurationProvider 
                            "NEW Consultingwerk.Framework.XmlConfigurationProvider ('.applicationsettings.xml':U)"} .  
&ENDIF
      
/* Mike Fechner, Consultingwerk Ltd. 26.01.2012
   Load services using ServiceLoader */
DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO .
   
oLoader = NEW ServiceLoader () .
oLoader:Load ("Consultingwerk/Framework/Server/services.xml":U) .   

&IF DEFINED (SmartFramework) NE 0 &THEN
oLoader:Load ("Consultingwerk/SmartFramework/services_server.xml":U) .   
&ENDIF

DELETE OBJECT oLoader .

IF FrameworkSettings:DebugMode THEN 
    LogManager:WriteFormattedMessage ("Startup propath: &1":U, FrameworkSettings:StartupPropath) .
 
CATCH err AS Progress.Lang.Error :
    Consultingwerk.Util.LogManager:WriteError (err) .		
    
    UNDO, THROW err. /* will prevent AppServer from starting */
END CATCH.
