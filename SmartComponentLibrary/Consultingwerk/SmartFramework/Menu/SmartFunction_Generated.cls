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
    File        : SmartFunction
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

CLASS Consultingwerk.SmartFramework.Menu.SmartFunction_Generated 
    INHERITS EntityTable
    ABSTRACT: 

    DEFINE PUBLIC PROPERTY FunctionCallParameter AS LONGCHAR  NO-UNDO
    GET.
    PUBLIC SET (arg as LONGCHAR):
        THIS-OBJECT:FunctionCallParameter = arg .
        THIS-OBJECT:SetModified () .
        OnFunctionCallParameterChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the FunctionCallParameter property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FunctionCallParameterChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FunctionCallParameterChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFunctionCallParameterChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:FunctionCallParameterChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY FunctionDescription AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:FunctionDescription = arg .
        THIS-OBJECT:SetModified () .
        OnFunctionDescriptionChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the FunctionDescription property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FunctionDescriptionChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FunctionDescriptionChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFunctionDescriptionChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:FunctionDescriptionChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

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

    DEFINE PUBLIC PROPERTY FunctionLargeImage AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:FunctionLargeImage = arg .
        THIS-OBJECT:SetModified () .
        OnFunctionLargeImageChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the FunctionLargeImage property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FunctionLargeImageChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FunctionLargeImageChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFunctionLargeImageChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:FunctionLargeImageChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY FunctionModuleGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:FunctionModuleGuid = arg .
        THIS-OBJECT:SetModified () .
        OnFunctionModuleGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the FunctionModuleGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FunctionModuleGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FunctionModuleGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFunctionModuleGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:FunctionModuleGuidChanged:Publish (THIS-OBJECT, e) . 
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

    DEFINE PUBLIC PROPERTY FunctionSmallImage AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:FunctionSmallImage = arg .
        THIS-OBJECT:SetModified () .
        OnFunctionSmallImageChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the FunctionSmallImage property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FunctionSmallImageChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FunctionSmallImageChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFunctionSmallImageChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:FunctionSmallImageChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY ModuleName AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:ModuleName = arg .
        THIS-OBJECT:SetModified () .
        OnModuleNameChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the ModuleName property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ModuleNameChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the ModuleNameChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnModuleNameChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:ModuleNameChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartFunction_Generated class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartFunction_Generated ():
    
    
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartFunction_Generated class                                                                      
        Notes:                                         
        @param phBuffer A source buffer to copy the values from                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartFunction_Generated (phBuffer AS HANDLE):
    
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

        COPY-LOB FROM phBuffer::FunctionCallParameter TO THIS-OBJECT:FunctionCallParameter .

        ASSIGN THIS-OBJECT:FunctionDescription = phBuffer::FunctionDescription .

        ASSIGN THIS-OBJECT:FunctionGuid = phBuffer::FunctionGuid .

        ASSIGN THIS-OBJECT:FunctionLargeImage = phBuffer::FunctionLargeImage .

        ASSIGN THIS-OBJECT:FunctionModuleGuid = phBuffer::FunctionModuleGuid .

        ASSIGN THIS-OBJECT:FunctionName = phBuffer::FunctionName .

        ASSIGN THIS-OBJECT:FunctionSmallImage = phBuffer::FunctionSmallImage .

        ASSIGN THIS-OBJECT:ModuleName = phBuffer::ModuleName .



    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Copies the entity table's properties to the buffer values                                                                         
        Notes:                      
        @param phBuffer The handle of the record buffer to assign properties from                                                             
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID AssignToBuffer (phBuffer AS HANDLE):
    
        COPY-LOB FROM THIS-OBJECT:FunctionCallParameter TO phBuffer::FunctionCallParameter .

        ASSIGN phBuffer::FunctionDescription = THIS-OBJECT:FunctionDescription .

        ASSIGN phBuffer::FunctionGuid = THIS-OBJECT:FunctionGuid .

        ASSIGN phBuffer::FunctionLargeImage = THIS-OBJECT:FunctionLargeImage .

        ASSIGN phBuffer::FunctionModuleGuid = THIS-OBJECT:FunctionModuleGuid .

        ASSIGN phBuffer::FunctionName = THIS-OBJECT:FunctionName .

        ASSIGN phBuffer::FunctionSmallImage = THIS-OBJECT:FunctionSmallImage .

        ASSIGN phBuffer::ModuleName = THIS-OBJECT:ModuleName .

    
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the SmartFunction_Generated class                                                                      
        Notes:                                                                        
        @param phBuffer The handle of the record buffer to assign fields to 
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartFunction_Generated ():

    END DESTRUCTOR.

END CLASS.
