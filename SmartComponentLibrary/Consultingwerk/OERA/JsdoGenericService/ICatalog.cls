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
    File        : ICatalog
    Purpose     : Interface for classes that can generate the JSON/JSDO 
                  Catalog file dynamically
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 19 11:53:54 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.JsdoGenericService.* FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH.

INTERFACE Consultingwerk.OERA.JsdoGenericService.ICatalog:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the Catalog for a given Business Entity 
        Notes:   
        @param pcEntityName The name of the Business Entity 
        @param poAddress The RestServiceAddress to include in the catalog output
        @param pcJsonCatalog OUTPUT The JSON Catalog for the Business Entity 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetCatalogForBusinessEntity (pcEntityName AS CHARACTER,
                                                    poAddress AS RestServiceAddress,
                                                    OUTPUT pcJsonCatalog AS LONGCHAR) .  
  
END INTERFACE.