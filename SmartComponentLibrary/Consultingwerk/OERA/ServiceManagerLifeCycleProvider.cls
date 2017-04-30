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
    File        : ServiceManagerLifeCycleProvider
    Purpose     : Services that provide service life cycle definition to
                  the ServiceManagerImpl class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 28 23:00:27 CEST 2016
    Notes       : Imports life cycle defintions from XML representation of
                  temp-table from Consultingwerk/OERA/ttServiceLifeCycleDefinition.i
                  Expected file name: ".servicelifecycledefinition.xml" or
                  the value of the "ServiceLifeCycleDefinition" setting from
                  the IConfigurationProvider service
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.Framework.* FROM PROPATH .
USING Consultingwerk.OERA.*      FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.OERA.ServiceManagerLifeCycleProvider
    IMPLEMENTS IServiceManagerLifeCycleProvider:

    {Consultingwerk/OERA/ttServiceLifeCycleDefinition.i}

    /**
     * Purpose: Returns the temp-table with Service Life Cycle definitions to the caller
     * Notes:   From Consultingwerk/OERA/ttServiceLifeCycleDefinition.i
     * @param ttServiceLifeCycleDefinition OUTPUT Temp-Table
     */
    METHOD PUBLIC VOID GetDefinitions (OUTPUT TABLE FOR ttServiceLifeCycleDefinition):

        DEFINE VARIABLE cServiceLifeCycleDefinition AS CHARACTER              NO-UNDO INITIAL ".servicelifecycledefinition.xml":U.
        DEFINE VARIABLE oConfigurationProvider      AS IConfigurationProvider NO-UNDO .

        EMPTY TEMP-TABLE ttServiceLifeCycleDefinition .

        ASSIGN oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        IF VALID-OBJECT (oConfigurationProvider) THEN DO:
            ASSIGN cServiceLifeCycleDefinition = oConfigurationProvider:GetValue("ServiceLifeCycleDefinition":U) .

            IF cServiceLifeCycleDefinition > "":U THEN
                FileAssert:Exists (cServiceLifeCycleDefinition) .
            ELSE
                ASSIGN cServiceLifeCycleDefinition = ".servicelifecycledefinition.xml":U .

        END.

        IF FileHelper:Exists (cServiceLifeCycleDefinition) THEN
            TEMP-TABLE ttServiceLifeCycleDefinition:READ-XML ("file":U,
                                                              FileHelper:FindFile(cServiceLifeCycleDefinition),
                                                              "empty":U,
                                                              ?,
                                                              ?) .
    END METHOD.

END CLASS.
