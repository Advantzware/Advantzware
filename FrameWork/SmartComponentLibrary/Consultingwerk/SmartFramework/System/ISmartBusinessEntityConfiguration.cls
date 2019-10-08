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
    File        : ISmartBusinessEntityConfiguration
    Purpose     : Interface for classes representing the SmartBusinessEntity 
                  data, by default represents the SmartDB.SmartBusinessEntity 
                  table, but may be implemented differently 
    Syntax      : Accessed through the IBusinessEntityConfigurationProvider 
                  service 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 03 18:58:15 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.System.ISmartBusinessEntityConfiguration: 

    /*------------------------------------------------------------------------------
        Purpose: Gets the BusinessEntityPackage name
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityPackage AS CHARACTER NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Gets the BusinessEntityName
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the current BusinessEntityPurpose value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityPurpose AS CHARACTER NO-UNDO
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the current DBRequired value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DBRequired AS LOGICAL NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the current TrackDeletions value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TrackDeletions AS LOGICAL NO-UNDO
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Gets the list of ISmartBusinessEntityTableConfiguration instances 
                 for this SmartBusinessEntity
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tables AS ListSmartBusinessEntityTableConfiguration NO-UNDO
    GET .

END INTERFACE.
