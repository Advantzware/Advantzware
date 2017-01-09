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
    File        : ToolbarItemSecurityService
    Purpose     : Security Service for the Toolbar item security realm
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 06 13:06:52 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.RepositoryServices.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*        FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.ToolbarItemSecurityService 
    IMPLEMENTS ISecurityRealmService: 

    /*------------------------------------------------------------------------------
        Purpose: Returns if an empty value for the security item key is allowed
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EmptyKeyAllowed AS LOGICAL NO-UNDO INIT FALSE  
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the properties of the SmartBusinessEntityLookupDescriptor 
                 for the Security Realm
        Notes:   
        @param poLookupDescriptor The reference to the SmartBusinessEntityLookupDescriptor to assign the properties to 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignLookupDescriptorProperties (poLookupDescriptor AS SmartBusinessEntityLookupDescriptor):
        
        ASSIGN poLookupDescriptor:AppServerPartition          = "":U
               poLookupDescriptor:LookupBrowserFields         = "ObjectName,ToolbarItemKey":U
               poLookupDescriptor:LookupControls              = "":U
               poLookupDescriptor:LookupDialogClassName       = "":U
               poLookupDescriptor:LookupDialogFilterFields    = "ObjectName,ToolbarItemKey":U
               poLookupDescriptor:LookupDialogFilterOperators = "BEGINS,BEGINS":U
               poLookupDescriptor:LookupDialogQuerySort       = "BY eSmartToolbarSecurityItem.ObjectName":U
               poLookupDescriptor:LookupDialogQueryString     = "FOR EACH eSmartToolbarSecurityItem":U
               poLookupDescriptor:LookupDialogTitle           = "Select Toolbar Item":U
               poLookupDescriptor:LookupEntityName            = "Consultingwerk.SmartFramework.Authorization.ToolbarSecurityItemBusinessEntity":U
               poLookupDescriptor:LookupEntityTable           = "eSmartToolbarSecurityItem":U
               poLookupDescriptor:LookupEntityView            = "":U
               poLookupDescriptor:LookupFields                = "":U
               poLookupDescriptor:LookupKeyField              = "ToolbarItemKey":U
               poLookupDescriptor:LookupKeyValueColumn        = "ToolbarSecurityItemGuid":U
               poLookupDescriptor:LookupQuerySort             = "":U
               poLookupDescriptor:LookupQueryString           = "FOR EACH eSmartToolbarSecurityItem WHERE eSmartToolbarSecurityItem.ToolbarItemKey BEGINS ~"&1~"":U
               .
               
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Description for the security item 
        Notes:   
        @param pcItemKey The unique key of the security item
        @return The description of the security item
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetDescription (pcItemKey AS CHARACTER):
        
        DEFINE VARIABLE oToolbarSecurityItemDatasetModel AS ToolbarSecurityItemDatasetModel NO-UNDO .
        
        IF pcItemKey = "":U THEN 
            RETURN "<unknown>":U .
        
        oToolbarSecurityItemDatasetModel = NEW ToolbarSecurityItemDatasetModel (pcItemKey) .
        
        IF oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:Available THEN DO:
            RETURN TRIM (SUBSTITUTE ("&1 &2":U, 
                                     oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:ObjectName,
                                     oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:ToolbarItemKey)) . 
        END .
        ELSE 
            RETURN "<unknown>":U . 
        
        RETURN pcItemKey .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Validates the security item key  
        Notes:   
        @param pcItemKey The unique key of the security item
        @return Logical value indicating if the security item key is valid
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateKey (pcItemKey AS CHARACTER):

        DEFINE VARIABLE oToolbarSecurityItemDatasetModel AS ToolbarSecurityItemDatasetModel NO-UNDO .
        
        IF pcItemKey = "":U THEN 
            RETURN FALSE .
        
        oToolbarSecurityItemDatasetModel = NEW ToolbarSecurityItemDatasetModel (pcItemKey) .
                
        RETURN oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:Available .

    END METHOD .

END CLASS.
