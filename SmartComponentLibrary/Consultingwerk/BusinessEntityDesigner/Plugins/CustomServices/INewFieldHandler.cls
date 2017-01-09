/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : INewTempTableHandler
    Purpose     : Service Interface for classes that provide call back 
                  functionality when a new Field has been created
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 25 19:32:17 CET 2014
    Notes       : Allows modification of the eField records
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.INewFieldHandler:  
  
    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i }
  
    /*------------------------------------------------------------------------------
        Purpose: Callback method that allows modification of the Business Entity Model
                 eField record after a new Field has been created from a Dataset field
        Notes:   Allows modification of the eField record
        @param pcTempTableName The name of the temp-table that contains the new field
        @param pcFieldName The name of the new field
        @param dsBusinessEntity INPUT-OUTPUT DATASET The Businesss Entity Dataset (should be passed BY-REFERENCE)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID HandleNewField (pcTempTableName AS CHARACTER,
                                       pcFieldName AS CHARACTER, 
                                       INPUT-OUTPUT DATASET dsBusinessEntity).

END INTERFACE.