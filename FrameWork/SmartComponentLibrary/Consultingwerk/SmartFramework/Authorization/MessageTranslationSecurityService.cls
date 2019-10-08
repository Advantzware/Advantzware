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
    File        : MessageTranslationSecurityService
    Purpose     : Security Service for the Smart Message Transalation 
                  security realm
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 06 13:06:52 CET 2014
    Notes       : Translation maintenance is based on langauges
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.RepositoryServices.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*        FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.MessageTranslationSecurityService 
    IMPLEMENTS ISecurityRealmService: 

    /*------------------------------------------------------------------------------
        Purpose: Returns if an empty value for the security item key is allowed
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EmptyKeyAllowed AS LOGICAL NO-UNDO INIT TRUE  
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the properties of the SmartBusinessEntityLookupDescriptor 
                 for the Security Realm
        Notes:   
        @param poLookupDescriptor The reference to the SmartBusinessEntityLookupDescriptor to assign the properties to 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignLookupDescriptorProperties (poLookupDescriptor AS SmartBusinessEntityLookupDescriptor):
        
        ASSIGN poLookupDescriptor:AppServerPartition          = "":U
               poLookupDescriptor:LookupBrowserFields         = "LanguageIsoCode,LanguageName":U
               poLookupDescriptor:LookupControls              = "":U
               poLookupDescriptor:LookupDialogClassName       = "":U
               poLookupDescriptor:LookupDialogFilterFields    = "LanguageIsoCode,LanguageName":U
               poLookupDescriptor:LookupDialogFilterOperators = "BEGINS,BEGINS":U
               poLookupDescriptor:LookupDialogQuerySort       = "BY eSmartLanguage.LanguageName":U
               poLookupDescriptor:LookupDialogQueryString     = "FOR EACH eSmartLanguage":U
               poLookupDescriptor:LookupDialogTitle           = "Select Language":U
               poLookupDescriptor:LookupEntityName            = "Consultingwerk.SmartFramework.System.LanguageBusinessEntity":U
               poLookupDescriptor:LookupEntityTable           = "eSmartLanguage":U
               poLookupDescriptor:LookupEntityView            = "":U
               poLookupDescriptor:LookupFields                = "":U
               poLookupDescriptor:LookupKeyField              = "LanguageIsoCode":U
               poLookupDescriptor:LookupKeyValueColumn        = "LanguageGuid":U
               poLookupDescriptor:LookupQuerySort             = "":U
               poLookupDescriptor:LookupQueryString           = "FOR EACH eSmartLanguage WHERE eSmartLanguage.LanguageIsoCode BEGINS ~"&1~"":U
               .
               
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Description for the security item 
        Notes:   
        @param pcItemKey The unique key of the security item
        @return The description of the security item
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetDescription (pcItemKey AS CHARACTER):
        
        DEFINE VARIABLE oLanguageModel AS LanguageDatasetModel NO-UNDO .
        
        IF pcItemKey = "":U THEN 
            RETURN "<default>":U .
        
        oLanguageModel = NEW LanguageDatasetModel (pcItemKey) .
        
        IF oLanguageModel:SmartLanguage:Available THEN 
            RETURN SUBSTITUTE ("&1":U, 
                               oLanguageModel:SmartLanguage:LanguageIsoCode) . 
        
        RETURN pcItemKey .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Validates the security item key  
        Notes:   
        @param pcItemKey The unique key of the security item
        @return Logical value indicating if the security item key is valid
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateKey (pcItemKey AS CHARACTER):
        
        RETURN pcItemKey = "":U OR CAN-FIND (SmartLanguage WHERE SmartLanguage.LanguageGuid = pcItemKey) .

    END METHOD .

END CLASS.
