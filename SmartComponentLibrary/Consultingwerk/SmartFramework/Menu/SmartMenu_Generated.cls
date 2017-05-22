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
    File        : SmartMenu
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 28.04.2013 00:33:34 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.EntityTable FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.SmartMenu_Generated 
    INHERITS EntityTable
    ABSTRACT: 

    DEFINE PUBLIC PROPERTY SmartFunctions AS Consultingwerk.SmartFramework.Menu.ListSmartFunction NO-UNDO GET. PROTECTED SET. 

    DEFINE PUBLIC PROPERTY FunctionGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:FunctionGuid = arg .
        THIS-OBJECT:SetModified () .
        OnFunctionGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the FunctionGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FunctionGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FunctionGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFunctionGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:FunctionGuidChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY FunctionName AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:FunctionName = arg .
        THIS-OBJECT:SetModified () .
        OnFunctionNameChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the FunctionName property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FunctionNameChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FunctionNameChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFunctionNameChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:FunctionNameChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY HasChild AS LOGICAL  NO-UNDO
    GET.
    PUBLIC SET (arg as LOGICAL):
        THIS-OBJECT:HasChild = arg .
        THIS-OBJECT:SetModified () .
        OnHasChildChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the HasChild property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT HasChildChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the HasChildChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnHasChildChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:HasChildChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY MenuBeginsAGroup AS LOGICAL  NO-UNDO
    GET.
    PUBLIC SET (arg as LOGICAL):
        THIS-OBJECT:MenuBeginsAGroup = arg .
        THIS-OBJECT:SetModified () .
        OnMenuBeginsAGroupChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the MenuBeginsAGroup property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT MenuBeginsAGroupChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MenuBeginsAGroupChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnMenuBeginsAGroupChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:MenuBeginsAGroupChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY MenuGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:MenuGuid = arg .
        THIS-OBJECT:SetModified () .
        OnMenuGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the MenuGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT MenuGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MenuGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnMenuGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:MenuGuidChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY MenuLargeImage AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:MenuLargeImage = arg .
        THIS-OBJECT:SetModified () .
        OnMenuLargeImageChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the MenuLargeImage property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT MenuLargeImageChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MenuLargeImageChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnMenuLargeImageChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:MenuLargeImageChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY MenuName AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:MenuName = arg .
        THIS-OBJECT:SetModified () .
        OnMenuNameChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the MenuName property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT MenuNameChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MenuNameChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnMenuNameChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:MenuNameChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY MenuSequence AS INTEGER  NO-UNDO
    GET.
    PUBLIC SET (arg as INTEGER):
        THIS-OBJECT:MenuSequence = arg .
        THIS-OBJECT:SetModified () .
        OnMenuSequenceChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the MenuSequence property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT MenuSequenceChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MenuSequenceChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnMenuSequenceChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:MenuSequenceChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY MenuSmallImage AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:MenuSmallImage = arg .
        THIS-OBJECT:SetModified () .
        OnMenuSmallImageChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the MenuSmallImage property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT MenuSmallImageChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MenuSmallImageChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnMenuSmallImageChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:MenuSmallImageChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY MenuStructureType AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:MenuStructureType = arg .
        THIS-OBJECT:SetModified () .
        OnMenuStructureTypeChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the MenuStructureType property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT MenuStructureTypeChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MenuStructureTypeChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnMenuStructureTypeChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:MenuStructureTypeChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY ParentMenuGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:ParentMenuGuid = arg .
        THIS-OBJECT:SetModified () .
        OnParentMenuGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the ParentMenuGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ParentMenuGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the ParentMenuGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnParentMenuGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:ParentMenuGuidChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartMenu_Generated class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenu_Generated ():
    
        THIS-OBJECT:SmartFunctions = NEW Consultingwerk.SmartFramework.Menu.ListSmartFunction () .

    
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartMenu_Generated class                                                                      
        Notes:                                         
        @param phBuffer A source buffer to copy the values from                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenu_Generated (phBuffer AS HANDLE):
    
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

        ASSIGN THIS-OBJECT:FunctionGuid = phBuffer::FunctionGuid .

        ASSIGN THIS-OBJECT:FunctionName = phBuffer::FunctionName .

        ASSIGN THIS-OBJECT:HasChild = phBuffer::HasChild .

        ASSIGN THIS-OBJECT:MenuBeginsAGroup = phBuffer::MenuBeginsAGroup .

        ASSIGN THIS-OBJECT:MenuGuid = phBuffer::MenuGuid .

        ASSIGN THIS-OBJECT:MenuLargeImage = phBuffer::MenuLargeImage .

        ASSIGN THIS-OBJECT:MenuName = phBuffer::MenuName .

        ASSIGN THIS-OBJECT:MenuSequence = phBuffer::MenuSequence .

        ASSIGN THIS-OBJECT:MenuSmallImage = phBuffer::MenuSmallImage .

        ASSIGN THIS-OBJECT:MenuStructureType = phBuffer::MenuStructureType .

        ASSIGN THIS-OBJECT:ParentMenuGuid = phBuffer::ParentMenuGuid .

        THIS-OBJECT:SmartFunctions:Clear () .
        THIS-OBJECT:GetChildRecords (phBuffer,
                                     "eSmartMenueSmartFunctionRelation":U,
                                     THIS-OBJECT:SmartFunctions,
                                     "Consultingwerk.SmartFramework.Menu.SmartFunction":U) .



    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Copies the entity table's properties to the buffer values                                                                         
        Notes:                      
        @param phBuffer The handle of the record buffer to assign properties from                                                             
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID AssignToBuffer (phBuffer AS HANDLE):
    
        ASSIGN phBuffer::FunctionGuid = THIS-OBJECT:FunctionGuid .

        ASSIGN phBuffer::FunctionName = THIS-OBJECT:FunctionName .

        ASSIGN phBuffer::HasChild = THIS-OBJECT:HasChild .

        ASSIGN phBuffer::MenuBeginsAGroup = THIS-OBJECT:MenuBeginsAGroup .

        ASSIGN phBuffer::MenuGuid = THIS-OBJECT:MenuGuid .

        ASSIGN phBuffer::MenuLargeImage = THIS-OBJECT:MenuLargeImage .

        ASSIGN phBuffer::MenuName = THIS-OBJECT:MenuName .

        ASSIGN phBuffer::MenuSequence = THIS-OBJECT:MenuSequence .

        ASSIGN phBuffer::MenuSmallImage = THIS-OBJECT:MenuSmallImage .

        ASSIGN phBuffer::MenuStructureType = THIS-OBJECT:MenuStructureType .

        ASSIGN phBuffer::ParentMenuGuid = THIS-OBJECT:ParentMenuGuid .

    
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the SmartMenu_Generated class                                                                      
        Notes:                                                                        
        @param phBuffer The handle of the record buffer to assign fields to 
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartMenu_Generated ():

    END DESTRUCTOR.

END CLASS.
