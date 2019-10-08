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
                  functionality when a new Temp-Table has been created
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 25 19:32:17 CET 2014
    Notes       : Allows modification of the eTable, eBusinessEntity record as well 
                  as the eField records
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.INewTempTableHandler:  
  
    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i }
  
    /*------------------------------------------------------------------------------
        Purpose: Callback method that allows modification of the Business Entity Model
                 after a new Table has been created
        Notes:   Allows modification of the eTable, eBusinessEntity record as well 
                 as the eField records
        @param pcTempTableName The name of the new temp-table
        @param dsBusinessEntity INPUT-OUTPUT DATASET The Businesss Entity Dataset (should be passed BY-REFERENCE)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID HandleNewTempTable (pcTempTableName AS CHARACTER,
                                           INPUT-OUTPUT DATASET dsBusinessEntity).

END INTERFACE.