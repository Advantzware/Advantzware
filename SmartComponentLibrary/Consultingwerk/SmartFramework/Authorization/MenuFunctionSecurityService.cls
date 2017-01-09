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
    File        : MenuFunctionSecurityService
    Purpose     : Security Service for the Smart Menu Function security
                  realm
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 28 16:05:15 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.RepositoryServices.* FROM PROPATH . 
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.MenuFunctionSecurityService 
    IMPLEMENTS ISecurityRealmService: 

    /*------------------------------------------------------------------------------
        Purpose: Returns if an empty value for the security item key is allowed
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EmptyKeyAllowed AS LOGICAL NO-UNDO INIT FALSE  
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuFunctionSecurityService class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuFunctionSecurityService ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the properties of the SmartBusinessEntityLookupDescriptor 
                 for the Security Realm
        Notes:   
        @param poLookupDescriptor The reference to the SmartBusinessEntityLookupDescriptor to assign the properties to 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignLookupDescriptorProperties (poLookupDescriptor AS SmartBusinessEntityLookupDescriptor):
        
        ASSIGN poLookupDescriptor:AppServerPartition          = "":U
               poLookupDescriptor:LookupBrowserFields         = "FunctionName,FunctionDescription":U
               poLookupDescriptor:LookupControls              = "":U
               poLookupDescriptor:LookupDialogClassName       = "":U
               poLookupDescriptor:LookupDialogFilterFields    = "FunctionName":U
               poLookupDescriptor:LookupDialogFilterOperators = "BEGINS":U
               poLookupDescriptor:LookupDialogQuerySort       = "BY eSmartFunction.FunctionName":U
               poLookupDescriptor:LookupDialogQueryString     = "FOR EACH eSmartFunction":U
               poLookupDescriptor:LookupDialogTitle           = "Select Menu Function":U
               poLookupDescriptor:LookupEntityName            = "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U
               poLookupDescriptor:LookupEntityTable           = "eSmartFunction":U
               poLookupDescriptor:LookupEntityView            = "":U
               poLookupDescriptor:LookupFields                = "":U
               poLookupDescriptor:LookupKeyField              = "FunctionName":U
               poLookupDescriptor:LookupKeyValueColumn        = "FunctionGuid":U
               poLookupDescriptor:LookupQuerySort             = "":U
               poLookupDescriptor:LookupQueryString           = "FOR EACH eSmartFunction WHERE eSmartFunction.FunctionName BEGINS ~"&1~"":U
               .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Description for the security item 
        Notes:   
        @param pcItemKey The unique key of the security item
        @return The description of the security item
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetDescription (pcItemKey AS CHARACTER):
        
        FIND SmartFunction WHERE SmartFunction.FunctionGuid = pcItemKey NO-LOCK NO-ERROR .
        
        IF AVAILABLE SmartFunction THEN 
            RETURN SmartFunction.FunctionName . 
        ELSE 
            RETURN "":U .  

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Validates the security item key  
        Notes:   
        @param pcItemKey The unique key of the security item
        @return Logical value indicating if the security item key is valid
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateKey (pcItemKey AS CHARACTER):
        
        RETURN CAN-FIND (SmartFunction WHERE SmartFunction.FunctionGuid = pcItemKey) .

    END METHOD .

END CLASS.
