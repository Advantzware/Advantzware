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
    File        : SampleNewFieldHandler
    Purpose     : Sample service class that provides call back 
                  functionality when a new Temp-Table Fields have been created
                  from a database field
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 25 19:38:51 CET 2014
    Notes       : Allows modification of the eField records
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.BusinessEntityDesigner.Services.*               FROM PROPATH .                     
USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.SampleNewFieldHandler 
    IMPLEMENTS INewFieldHandler: 

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
                                       INPUT-OUTPUT DATASET dsBusinessEntity):
        
        FIND eField WHERE eField.TempTableName = pcTempTableName 
                      AND eField.FieldName     = pcFieldName .   
            
        ASSIGN eField.FieldName = UPPER (eField.FieldName) .

    END METHOD .

END CLASS.
