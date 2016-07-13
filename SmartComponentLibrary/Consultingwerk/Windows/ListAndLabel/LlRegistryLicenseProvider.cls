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
    File        : LlRegistryLicenseProvider
    Purpose     : Provides List & Label Registration information from the
                  ISettingsService implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 23 22:54:15 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*            FROM PROPATH . 
USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.LlRegistryLicenseProvider 
    IMPLEMENTS ILlLicenseProvider: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List & Label License Info
        Notes:   See List & Label Documentation, LL_OPTIONSTR_LICENSINGINFO
        @return The list an Label License Info, usually a 6 character string
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetLicenseInfo ():
        
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO . 
        
        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService 
                                                         "NEW Consultingwerk.Windows.RegistrySettingsService()"} .

        IF NOT VALID-OBJECT (oSettingsService) THEN 
            RETURN "":U . 
        
        RETURN oSettingsService:GetSetting ("":U, 
                                            "LlLicenseInfo":U) .
    END METHOD .

END CLASS.
