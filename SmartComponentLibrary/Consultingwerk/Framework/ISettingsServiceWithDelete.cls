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
    File        : ISettingsServiceWithDelete
    Purpose     : Interface for Settings Service implementations that support
                  deletion of single Settings or whole Setting Keys
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 04 07:20:51 CET 2015
    Notes       : Allows abstraction from writing to the Registry or 
                  actually using a different implementation on the client
                  or on the AppServer
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH . 
USING Progress.Lang.*            FROM PROPATH .

INTERFACE Consultingwerk.Framework.ISettingsServiceWithDelete:  
  
    /*------------------------------------------------------------------------------
        Purpose: Deletes a settings value
        Notes:      
        @param pcBaseKey The settings base key
        @param pcKey The settings key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteSetting (pcBaseKey AS CHARACTER,
                                      pcKey AS CHARACTER) .
    
    /*------------------------------------------------------------------------------
        Purpose: Deletes a settings value
        Notes:      
        @param pcBaseKey The settings base key to delete
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteSettings (pcBaseKey AS CHARACTER) .
    
END INTERFACE.
