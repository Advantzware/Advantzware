/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SystemDescriptionProvider
    Purpose     : Interface for classes providing access to the Description 
                  of the current system (e.g. Dev, Test, Prod)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 26 07:52:36 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH.

INTERFACE Consultingwerk.Framework.ISystemDescriptionProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the ISystemDescription instance that describes the current
                 system 
        Notes:   May return ? when no system description is available, should not 
                 throw an error in that case
        @return The ISystemDescription instance that describes the current system or ? 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ISystemDescription GetSystemDescription () .

END INTERFACE .
