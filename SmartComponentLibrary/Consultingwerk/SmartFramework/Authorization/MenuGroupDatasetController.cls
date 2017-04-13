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
    File        : MenuGroupDatasetController
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories
    Created     : 11.10.2012 00:37:21
    Notes       : Conditionally implements the IDatasetController Interface
                  when the SmartComponentLibrary preprocessor option is set.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartComponents.Interfaces.* .
USING Consultingwerk.SmartFramework.Authorization.* .
USING Progress.Lang.*.

CLASS Consultingwerk.SmartFramework.Authorization.MenuGroupDatasetController 
    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN IMPLEMENTS IDatasetController &ENDIF : 

    { Consultingwerk/SmartFramework/Authorization/dsMenuGroup.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the static dataset defined inside the
                 Dataset Controller                                                                     
        Notes:   Interface IDatasetController member                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuGroupDatasetController class                                                                      
        Notes:   Assigns the DatasetHandle property                                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuGroupDatasetController ():
        SUPER ().

        THIS-OBJECT:DatasetHandle = DATASET dsMenuGroup:HANDLE .         
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Assignes a group to a menu
        Notes:   
        @param pcSelectedMenuGroupGuid Unique identifier of the ePossibleGroup
               row to be transfered to eSmartMenuGroup
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignGroupToMenu (pcSelectedMenuGroupGuid AS CHARACTER):
        
        FIND FIRST ePossibleGroup WHERE ePossibleGroup.MenuGroupGuid = pcSelectedMenuGroupGuid NO-LOCK NO-ERROR.

        CREATE eSmartMenuGroup.
        
        BUFFER-COPY ePossibleGroup TO eSmartMenuGroup.
        
        RELEASE eSmartMenuGroup.
        
        DELETE ePossibleGroup.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when detaching from the Dataset of the DatasetController.
                 Allows event unsubscription etc.                                                                      
        Notes:   Interface IDatasetController member                                                                        
        @param poConsumer The reference of the Object detaching from the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (poConsumer AS Progress.Lang.Object):
        
    END METHOD.
        
    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when attaching to the Dataset of the DatasetController.
                 Allows event subscription etc.                                                                     
        Notes:   Interface IDatasetController member                                                                        
        @param poConsumer The reference of the Object attaching to the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Unassignes a group from a menu
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UnAssignGroupFromMenu ():
        
        CREATE ePossibleGroup.
        
        BUFFER-COPY eSmartMenuGroup TO ePossibleGroup.
        
        RELEASE ePossibleGroup.
        
        DELETE eSmartMenuGroup.

    END METHOD .

END CLASS.