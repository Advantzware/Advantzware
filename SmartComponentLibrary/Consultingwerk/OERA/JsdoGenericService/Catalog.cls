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
    File        : Catalog
    Purpose     : Generic JSON/JSDO Catalog generation 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 19 11:09:49 CEST 2015
    Notes       : Calls into version specific implementations (like V1.2 for 11.5) 
                  Currently always calls into the V1.2 implementation
  ----------------------------------------------------------------------*/

@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*               FROM PROPATH .
USING Consultingwerk.Framework.*               FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.* FROM PROPATH .  
USING Consultingwerk.Util.*                    FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.OERA.JsdoGenericService.Catalog: 

    DEFINE VARIABLE oConfig AS IConfigurationProvider NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Catalog class
        Notes:   Initializes IConfigurationProvider service based on .restapplicationsettings
                 when the service should not yet be deployed
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC Catalog ():
	    
		DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

		SUPER ().
		
        FileAssert:Exists (".restapplicationsettings":U) .
		
        ASSIGN cFileName = FileHelper:FindFile (".restapplicationsettings":U) .
		
		ASSIGN oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider
		                                               "NEW ConfigurationProvider (cFileName)"} .
		
	END CONSTRUCTOR.


    /*------------------------------------------------------------------------------
        Purpose: Returns the Catalog for a given Business Entity 
        Notes:   
        @param pcEntityName The name of the Business Entity 
        @param pcJsonCatalog OUTPUT The JSON Catalog for the Business Entity 
    ------------------------------------------------------------------------------*/
	@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
	METHOD PUBLIC VOID GetCatalogForBusinessEntity (pcEntityName AS CHARACTER,
	                                                OUTPUT pcJsonCatalog AS LONGCHAR):
		
        DEFINE VARIABLE oCatalog         AS ICatalog                    NO-UNDO .
        DEFINE VARIABLE oAddressProvider AS IRestServiceAddressProvider NO-UNDO . 

        oAddressProvider = {Consultingwerk/get-service.i Consultingwerk.OERA.JsdoGenericService.IRestServiceAddressProvider
                                                         "NEW Consultingwerk.OERA.JsdoGenericService.RestServiceAddressProvider()"} .

        oCatalog = NEW CatalogV12 () .

        oCatalog:GetCatalogForBusinessEntity (pcEntityName,
                                              oAddressProvider:GetRestServiceAddress (pcEntityName, 
                                                                                      oConfig:GetValue("RestServiceName":U),
                                                                                      oConfig:GetValue("RestServiceAddress":U)),
                                              OUTPUT pcJsonCatalog) .

	END METHOD .

END CLASS.
