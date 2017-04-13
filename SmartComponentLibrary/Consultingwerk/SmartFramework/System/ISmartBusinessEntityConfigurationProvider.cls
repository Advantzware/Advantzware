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
    File        : ISmartBusinessEntityConfigurationProvider
    Purpose     : Interface for services returning ISmartBusinessEntityConfiguration 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 03 19:26:24 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.System.ISmartBusinessEntityConfigurationProvider:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the ISmartBusinessEntityConfiguration instance for the a 
                 Business Entity
        Notes:   Returns ? when no configuration exists
        @param pcBusinessEntityName The full class name (including package) of the SmartBusinessEntity
        @return The ISmartBusinessEntityConfiguration instance for the requested Business Entity
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ISmartBusinessEntityConfiguration GetSmartBusinessEntityConfiguration (pcBusinessEntityName AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Resets the services cache (if present) 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ResetCache ().

END INTERFACE.
