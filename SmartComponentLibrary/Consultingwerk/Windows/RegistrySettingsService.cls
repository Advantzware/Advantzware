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
    File        : RegistrySettingsServie
    Purpose     : Stores user settings to the Windows registry
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 03 21:29:22 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Windows.RegistrySettingsService 
    IMPLEMENTS ISettingsService, ISettingsServiceLong, ISettingsServiceWithDelete: 

    /*------------------------------------------------------------------------------
        Purpose: Deletes a settings value
        Notes:      
        @param pcBaseKey The settings base key
        @param pcKey The settings key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteSetting (pcBaseKey AS CHARACTER,
                                      pcKey AS CHARACTER) .
    
        Consultingwerk.Framework.Registry:DeleteRegistryValue ("User":U,
                                                               FrameworkSettings:BaseRegistryKey + pcBaseKey, 
                                                               pcKey) .
    
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Deletes a settings value
        Notes:      
        @param pcBaseKey The settings base key to delete
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteSettings (pcBaseKey AS CHARACTER) .
    
        Consultingwerk.Framework.Registry:DeleteRegistryKey ("User":U,
                                                             FrameworkSettings:BaseRegistryKey + pcBaseKey) . 
    
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a user setting from the registry                                                                      
        Notes:                
        @param pcBaseKey The settings base key
        @param pcKey The settings key
        @return The setting value                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetSetting (pcBaseKey AS CHARACTER, 
                                        pcKey AS CHARACTER):
        
        RETURN STRING (Consultingwerk.Framework.Registry:GetRegistryValue 
                        ("User":U,
                         FrameworkSettings:BaseRegistryKey + pcBaseKey, 
                         pcKey)) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a user setting from the registry                                                                      
        Notes:      
        @param pcBaseKey The settings base key
        @param pcKey The settings key
        @return The setting value                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GetSettingLong (pcBaseKey AS CHARACTER, 
                                           pcKey AS CHARACTER):
        
        RETURN Consultingwerk.Framework.Registry:GetRegistryValue 
                   ("User":U,
                    FrameworkSettings:BaseRegistryKey + pcBaseKey, 
                    pcKey) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores a user setting in the registry
        Notes:   
        @param pcBaseKey The settings base key
        @param pcKey The settings key
        @param pcValue The value to assign to the setting                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreSetting (pcBaseKey AS CHARACTER, 
                                     pcKey AS CHARACTER, 
                                     pcValue AS CHARACTER ):
        
        Consultingwerk.Framework.Registry:SetRegistryValue 
                       ("User":U,
                        FrameworkSettings:BaseRegistryKey + pcBaseKey, 
                        pcKey, 
                        pcValue) .

    END METHOD.
    /*------------------------------------------------------------------------------
        Purpose: Stores a user setting in the registry
        Notes:  
        @param pcBaseKey The settings base key
        @param pcKey The settings key
        @param pcValue The value to assign to the setting                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreSettingLong (pcBaseKey AS CHARACTER, 
                                         pcKey AS CHARACTER, 
                                         pcValue AS LONGCHAR):
        
        Consultingwerk.Framework.Registry:SetRegistryValue 
                       ("User":U,
                        FrameworkSettings:BaseRegistryKey + pcBaseKey, 
                        pcKey, 
                        pcValue) .

    END METHOD.

END CLASS.
