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
    File        : ISmartBusinessEntityTableConfiguration
    Purpose     : Interface for classes representing the SmartBusinessEntityTable 
                  data, by default represents the SmartDB.SmartBusinessEntityTable 
                  table, but may be implemented differently 
    Syntax      : Accessed through the IBusinessEntityConfigurationProvider 
                  service 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 03 19:02:21 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.System.ISmartBusinessEntityTableConfiguration:  

    /*------------------------------------------------------------------------------
        Purpose: Gets the TableOrder value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableOrder AS INTEGER NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the TableName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableName AS CHARACTER NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the TableLabelSingular value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableLabelSingular AS CHARACTER NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the TableLabelPlural value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableLabelPlural AS CHARACTER NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the ShouldUseBatching value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ShouldUseBatching AS LOGICAL NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the BatchSize value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BatchSize AS INTEGER NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets AllowDelete value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AllowDelete AS LOGICAL NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the AllowCreate value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AllowCreate AS LOGICAL NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the AllowUpdate value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AllowUpdate AS LOGICAL NO-UNDO
    GET .

END INTERFACE.
