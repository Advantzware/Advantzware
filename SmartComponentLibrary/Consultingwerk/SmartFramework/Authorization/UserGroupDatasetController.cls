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
    File        : UserGroupDatasetController
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 11.10.2012 00:37:56
    Notes       : Conditionally implements the IDatasetController Interface
                  when the SmartComponentLibrary preprocessor option is set.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartComponents.Interfaces.* .
USING Consultingwerk.SmartFramework.Authorization.* .
USING Progress.Lang.*.

CLASS Consultingwerk.SmartFramework.Authorization.UserGroupDatasetController 
    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN IMPLEMENTS IDatasetController &ENDIF : 

    { Consultingwerk/SmartFramework/Authorization/dsUserGroup.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the static dataset defined inside the
                 Dataset Controller                                                                     
        Notes:   Interface IDatasetController member                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UserGroupDatasetController class                                                                      
        Notes:   Assigns the DatasetHandle property                                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserGroupDatasetController ():
        SUPER ().

        THIS-OBJECT:DatasetHandle = DATASET dsUserGroup:HANDLE .         
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Assignes a user to a group
        Notes:   
        @param pcSelectedUserGroupGuid Unique identifier of the ePossibleAssignments
               row to be transfered to eSmartUserGroup
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID AssignUserToGroup (pcSelectedUserGroupGuid AS CHARACTER):
		
		FIND FIRST ePossibleAssignments WHERE ePossibleAssignments.UserGroupGuid = pcSelectedUserGroupGuid NO-LOCK NO-ERROR.

        CREATE eSmartUserGroup.
        
        BUFFER-COPY ePossibleAssignments TO eSmartUserGroup.
        
        RELEASE eSmartUserGroup.
        
        DELETE ePossibleAssignments.

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
        Purpose: Unassignes a user from a group
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID UnAssignUserFromGroup ():
		
        CREATE ePossibleAssignments.
        
        BUFFER-COPY eSmartUserGroup TO ePossibleAssignments.
        
        RELEASE ePossibleAssignments.
        
        DELETE eSmartUserGroup.

	END METHOD .

END CLASS.