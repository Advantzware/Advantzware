/**************************************************************"********
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
    File        : RestServerStartupValidationService
    Purpose     : Validates a REST AppServer session during startup.
                  Misconfigurated sessions will be stopped from starting
                  by throwing an error from the service method
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 28 08:11:44 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                  FROM PROPATH .
USING Consultingwerk.Framework.*        FROM PROPATH .
USING Consultingwerk.Framework.Server.* FROM PROPATH .
USING Consultingwerk.Util.*             FROM PROPATH .
USING Progress.Lang.*                   FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Framework.Server.RestServerStartupValidationService
    IMPLEMENTS IStartupValidationService:

    /**
     * Purpose: Validates that a configuration value is present
     * Notes:
     * @param pcSettingName The name of the configuration provider setting to test for
     */
    METHOD PUBLIC VOID TestConfigurationValue (pcSettingName AS CHARACTER):

        DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .
        DEFINE VARIABLE cValue                 AS CHARACTER              NO-UNDO .

        oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        cValue = oConfigurationProvider:GetValue (pcSettingName) .

        IF CharacterType:IsNullOrEmpty (cValue) THEN
            UNDO, THROW NEW StartupValidationFailedException (SUBSTITUTE ("Unable to start session without ~"&1~" configuration setting."{&TRAN},
                                                                          pcSettingName),
                                                              0) .
    END METHOD.

    /**
     * Purpose: Validates a session startup
     * Notes:   Expected to throw an error, when the session startup is
     *          cancelled due to misconfiguration
     */
    METHOD PUBLIC VOID ValidateSessionStartup ():

        DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .

        oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        IF NOT VALID-OBJECT (oConfigurationProvider) THEN
            UNDO, THROW NEW StartupValidationFailedException ("Unable to start session without IConfigurationProvider service."{&TRAN},
                                                              0) .

        THIS-OBJECT:TestConfigurationValue ("RestServiceName":U) .
        THIS-OBJECT:TestConfigurationValue ("RestServiceAddress":U) .

        LogManager:WriteMessage("Startup Validation succeeded."{&TRAN}) .

    END METHOD.

END CLASS.
