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
    File        : IEntityPackageNameProvider
    Purpose     : Service Interface for classes that can provide the 
                  EntityPackage information to the Business Entity Designer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 24 14:28:59 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH . 
USING Progress.Lang.*                                                FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.IEntityPackageNameProvider:  
  
    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i }
    
    /*------------------------------------------------------------------------------
        Purpose: Provides package information for the Entity Class of a temp-table
        Notes:
        @param pcTempTableName The name of the new temp-table
        @param dsBusinessEntity INPUT-OUTPUT DATASET The Businesss Entity Dataset (should be passed BY-REFERENCE)
        @return The Entity Package name            
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetEntityPackageNameForTable (pcTempTableName AS CHARACTER,
                                                          INPUT-OUTPUT DATASET dsBusinessEntity).

END INTERFACE.
