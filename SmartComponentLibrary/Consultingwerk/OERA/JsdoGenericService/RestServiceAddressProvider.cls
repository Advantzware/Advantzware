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
    File        : RestServiceAddressProvider
    Purpose     : Service that can provide the address portion of the JSDO 
                  catalog file
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 12 23:13:59 CEST 2015
    Notes       : SCL-874: The JSDO session cannot add multiple catalogs
                  with the same service name. This class can provide a 
                  distinct service name for a Business Entity
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.JsdoGenericService.* FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.OERA.JsdoGenericService.RestServiceAddressProvider 
    IMPLEMENTS IRestServiceAddressProvider: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the REST service address for a Business Entity
        Notes:   
        @param pcEntityName The Business Entity Name
        @param pcRestServiceName The RestServiceName from the configuration file 
        @param pcRestServiceAddress The RestServiceAddress from the configuration file 
        @return The RestServiceAddress instance for the business entity
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC RestServiceAddress GetRestServiceAddress (pcEntityName AS CHARACTER, 
                                                            pcRestServiceName AS CHARACTER, 
                                                            pcRestServiceAddress AS CHARACTER):
		
		RETURN NEW RestServiceAddress (SUBSTITUTE ("&1-&2":U, pcRestServiceName, pcEntityName),
                                       pcRestServiceAddress) .

	END METHOD .

END CLASS.
