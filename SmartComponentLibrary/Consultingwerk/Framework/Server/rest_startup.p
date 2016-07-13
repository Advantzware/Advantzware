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
    File        : rest_startup.p
    Purpose     : AppServer Startup Procedure for REST AppServer

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 26 23:55:01 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.Framework.*        FROM PROPATH .
USING Consultingwerk.OERA.*             FROM PROPATH .
USING Consultingwerk.Util.*             FROM PROPATH .

{Consultingwerk/products.i}

DEFINE INPUT  PARAMETER pcParameter AS CHARACTER NO-UNDO.

DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .

/* ***************************  Main Block  *************************** */

{Consultingwerk/agentstart-log.i}

IF ListHelper:CanDo (pcParameter, "DebugMode":U) THEN DO: 
    FrameworkSettings:DebugMode = TRUE .
    DataAccess:LogFetchDataDetails = TRUE . 
    LogManager:AddCustomLogEntries ("ServiceInterface,DataAccess,ServiceLoader,ConfigurationProvider,SessionManager":U) .
END.    
    
/* Only support the Service Interface path to the SmartComponent Library client */
IF ListHelper:CanDo (pcParameter, "OeraSiOnly":U) THEN
    SESSION:EXPORT ("{&OERASI}/*.*":U) .    
      
/* Mike Fechner, Consultingwerk Ltd. 20.01.2013
   Integration of the ConfigutaionProvider */
&IF PROVERSION NE "10.2B" &THEN    
/* Jsonbased config provider only from 11.x */
oConfigurationProvider = {Consultingwerk/get-service.i 
                            Consultingwerk.Framework.IConfigurationProvider 
                            "NEW Consultingwerk.Framework.ConfigurationProvider (FileHelper:FindFile ('.restapplicationsettings':U))"} .  
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
oLoader:Load ("Consultingwerk/Framework/Server/rest_services.xml":U) .

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
