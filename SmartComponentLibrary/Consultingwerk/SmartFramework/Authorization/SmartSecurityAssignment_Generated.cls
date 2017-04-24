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
    File        : SmartSecurityAssignment
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 28.07.2013 17:49:52 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.EntityTable FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SmartSecurityAssignment_Generated 
    INHERITS EntityTable
    ABSTRACT: 

    DEFINE PUBLIC PROPERTY GroupGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:GroupGuid = arg .
        THIS-OBJECT:SetModified () .
        OnGroupGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the GroupGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT GroupGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the GroupGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnGroupGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:GroupGuidChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY OwnerName AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:OwnerName = arg .
        THIS-OBJECT:SetModified () .
        OnOwnerNameChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the OwnerName property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT OwnerNameChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the OwnerNameChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnOwnerNameChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:OwnerNameChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY OwnerType AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:OwnerType = arg .
        THIS-OBJECT:SetModified () .
        OnOwnerTypeChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the OwnerType property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT OwnerTypeChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the OwnerTypeChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnOwnerTypeChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:OwnerTypeChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY Restricted AS LOGICAL  NO-UNDO
    GET.
    PUBLIC SET (arg as LOGICAL):
        THIS-OBJECT:Restricted = arg .
        THIS-OBJECT:SetModified () .
        OnRestrictedChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the Restricted property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT RestrictedChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the RestrictedChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnRestrictedChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:RestrictedChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY SecurityAssignmentGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:SecurityAssignmentGuid = arg .
        THIS-OBJECT:SetModified () .
        OnSecurityAssignmentGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the SecurityAssignmentGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SecurityAssignmentGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the SecurityAssignmentGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSecurityAssignmentGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:SecurityAssignmentGuidChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY SecurityItemDescription AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:SecurityItemDescription = arg .
        THIS-OBJECT:SetModified () .
        OnSecurityItemDescriptionChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the SecurityItemDescription property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SecurityItemDescriptionChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the SecurityItemDescriptionChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSecurityItemDescriptionChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:SecurityItemDescriptionChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY SecurityItemGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:SecurityItemGuid = arg .
        THIS-OBJECT:SetModified () .
        OnSecurityItemGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the SecurityItemGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SecurityItemGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the SecurityItemGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSecurityItemGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:SecurityItemGuidChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY SecurityRealmCode AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:SecurityRealmCode = arg .
        THIS-OBJECT:SetModified () .
        OnSecurityRealmCodeChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the SecurityRealmCode property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SecurityRealmCodeChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the SecurityRealmCodeChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSecurityRealmCodeChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:SecurityRealmCodeChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY SecurityRealmDescription AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:SecurityRealmDescription = arg .
        THIS-OBJECT:SetModified () .
        OnSecurityRealmDescriptionChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the SecurityRealmDescription property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SecurityRealmDescriptionChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the SecurityRealmDescriptionChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSecurityRealmDescriptionChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:SecurityRealmDescriptionChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY SecurityRealmGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:SecurityRealmGuid = arg .
        THIS-OBJECT:SetModified () .
        OnSecurityRealmGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the SecurityRealmGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SecurityRealmGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the SecurityRealmGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSecurityRealmGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:SecurityRealmGuidChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY UserGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:UserGuid = arg .
        THIS-OBJECT:SetModified () .
        OnUserGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the UserGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT UserGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the UserGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnUserGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:UserGuidChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartSecurityAssignment_Generated class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSecurityAssignment_Generated ():
    
    
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartSecurityAssignment_Generated class                                                                      
        Notes:                                         
        @param phBuffer A source buffer to copy the values from                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSecurityAssignment_Generated (phBuffer AS HANDLE):
    
        THIS-OBJECT () .
    
        Consultingwerk.Assertion.HandleAssert:WidgetType (phBuffer, 
                                                          "BUFFER":U) .     
    
        THIS-OBJECT:AssignFromBuffer (phBuffer) .  
    
    END CONSTRUCTOR .
        
    /*------------------------------------------------------------------------------
        Purpose: Copies the buffer values to the entity table's properties                                                                        
        Notes:       
        @param phBuffer The handle of the record buffer to assign properties from                                                             
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID AssignFromBuffer (phBuffer AS HANDLE):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        ASSIGN THIS-OBJECT:GroupGuid = phBuffer::GroupGuid .

        ASSIGN THIS-OBJECT:OwnerName = phBuffer::OwnerName .

        ASSIGN THIS-OBJECT:OwnerType = phBuffer::OwnerType .

        ASSIGN THIS-OBJECT:Restricted = phBuffer::Restricted .

        ASSIGN THIS-OBJECT:SecurityAssignmentGuid = phBuffer::SecurityAssignmentGuid .

        ASSIGN THIS-OBJECT:SecurityItemDescription = phBuffer::SecurityItemDescription .

        ASSIGN THIS-OBJECT:SecurityItemGuid = phBuffer::SecurityItemGuid .

        ASSIGN THIS-OBJECT:SecurityRealmCode = phBuffer::SecurityRealmCode .

        ASSIGN THIS-OBJECT:SecurityRealmDescription = phBuffer::SecurityRealmDescription .

        ASSIGN THIS-OBJECT:SecurityRealmGuid = phBuffer::SecurityRealmGuid .

        ASSIGN THIS-OBJECT:UserGuid = phBuffer::UserGuid .



    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Copies the entity table's properties to the buffer values                                                                         
        Notes:                      
        @param phBuffer The handle of the record buffer to assign properties from                                                             
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID AssignToBuffer (phBuffer AS HANDLE):
    
        ASSIGN phBuffer::GroupGuid = THIS-OBJECT:GroupGuid .

        ASSIGN phBuffer::OwnerName = THIS-OBJECT:OwnerName .

        ASSIGN phBuffer::OwnerType = THIS-OBJECT:OwnerType .

        ASSIGN phBuffer::Restricted = THIS-OBJECT:Restricted .

        ASSIGN phBuffer::SecurityAssignmentGuid = THIS-OBJECT:SecurityAssignmentGuid .

        ASSIGN phBuffer::SecurityItemDescription = THIS-OBJECT:SecurityItemDescription .

        ASSIGN phBuffer::SecurityItemGuid = THIS-OBJECT:SecurityItemGuid .

        ASSIGN phBuffer::SecurityRealmCode = THIS-OBJECT:SecurityRealmCode .

        ASSIGN phBuffer::SecurityRealmDescription = THIS-OBJECT:SecurityRealmDescription .

        ASSIGN phBuffer::SecurityRealmGuid = THIS-OBJECT:SecurityRealmGuid .

        ASSIGN phBuffer::UserGuid = THIS-OBJECT:UserGuid .

    
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the SmartSecurityAssignment_Generated class                                                                      
        Notes:                                                                        
        @param phBuffer The handle of the record buffer to assign fields to 
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartSecurityAssignment_Generated ():

    END DESTRUCTOR.

END CLASS.
