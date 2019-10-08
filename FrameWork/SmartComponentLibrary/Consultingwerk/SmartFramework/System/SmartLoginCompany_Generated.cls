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
    File        : SmartLoginCompany
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 28.04.2013 00:33:34 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.EntityTable FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartLoginCompany_Generated 
    INHERITS EntityTable
    ABSTRACT: 

    DEFINE PUBLIC PROPERTY LoginCompanyGuid AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:LoginCompanyGuid = arg .
        THIS-OBJECT:SetModified () .
        OnLoginCompanyGuidChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyGuid property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LoginCompanyGuidChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyGuidChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLoginCompanyGuidChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:LoginCompanyGuidChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY LoginCompanyImageLarge AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:LoginCompanyImageLarge = arg .
        THIS-OBJECT:SetModified () .
        OnLoginCompanyImageLargeChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyImageLarge property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LoginCompanyImageLargeChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyImageLargeChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLoginCompanyImageLargeChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:LoginCompanyImageLargeChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY LoginCompanyImageSmall AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:LoginCompanyImageSmall = arg .
        THIS-OBJECT:SetModified () .
        OnLoginCompanyImageSmallChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyImageSmall property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LoginCompanyImageSmallChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyImageSmallChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLoginCompanyImageSmallChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:LoginCompanyImageSmallChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY LoginCompanyName AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:LoginCompanyName = arg .
        THIS-OBJECT:SetModified () .
        OnLoginCompanyNameChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyName property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LoginCompanyNameChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyNameChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLoginCompanyNameChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:LoginCompanyNameChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY LoginCompanyReferenceChar AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:LoginCompanyReferenceChar = arg .
        THIS-OBJECT:SetModified () .
        OnLoginCompanyReferenceCharChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyReferenceChar property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LoginCompanyReferenceCharChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyReferenceCharChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLoginCompanyReferenceCharChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:LoginCompanyReferenceCharChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY LoginCompanyReferenceDecimal AS DECIMAL  NO-UNDO
    GET.
    PUBLIC SET (arg as DECIMAL):
        THIS-OBJECT:LoginCompanyReferenceDecimal = arg .
        THIS-OBJECT:SetModified () .
        OnLoginCompanyReferenceDecimalChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyReferenceDecimal property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LoginCompanyReferenceDecimalChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyReferenceDecimalChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLoginCompanyReferenceDecimalChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:LoginCompanyReferenceDecimalChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY LoginCompanyReferenceInteger AS INTEGER  NO-UNDO
    GET.
    PUBLIC SET (arg as INTEGER):
        THIS-OBJECT:LoginCompanyReferenceInteger = arg .
        THIS-OBJECT:SetModified () .
        OnLoginCompanyReferenceIntegerChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyReferenceInteger property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LoginCompanyReferenceIntegerChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyReferenceIntegerChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLoginCompanyReferenceIntegerChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:LoginCompanyReferenceIntegerChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY LoginCompanyShort AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:LoginCompanyShort = arg .
        THIS-OBJECT:SetModified () .
        OnLoginCompanyShortChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyShort property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LoginCompanyShortChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyShortChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLoginCompanyShortChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:LoginCompanyShortChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY LoginCompanyTenantDomain AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:LoginCompanyTenantDomain = arg .
        THIS-OBJECT:SetModified () .
        OnLoginCompanyTenantDomainChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LoginCompanyTenantDomain property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LoginCompanyTenantDomainChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the LoginCompanyTenantDomainChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLoginCompanyTenantDomainChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:LoginCompanyTenantDomainChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartLoginCompany_Generated class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartLoginCompany_Generated ():
    
    
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartLoginCompany_Generated class                                                                      
        Notes:                                         
        @param phBuffer A source buffer to copy the values from                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartLoginCompany_Generated (phBuffer AS HANDLE):
    
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

        ASSIGN THIS-OBJECT:LoginCompanyGuid = phBuffer::LoginCompanyGuid .

        ASSIGN THIS-OBJECT:LoginCompanyImageLarge = phBuffer::LoginCompanyImageLarge .

        ASSIGN THIS-OBJECT:LoginCompanyImageSmall = phBuffer::LoginCompanyImageSmall .

        ASSIGN THIS-OBJECT:LoginCompanyName = phBuffer::LoginCompanyName .

        ASSIGN THIS-OBJECT:LoginCompanyReferenceChar = phBuffer::LoginCompanyReferenceChar .

        ASSIGN THIS-OBJECT:LoginCompanyReferenceDecimal = phBuffer::LoginCompanyReferenceDecimal .

        ASSIGN THIS-OBJECT:LoginCompanyReferenceInteger = phBuffer::LoginCompanyReferenceInteger .

        ASSIGN THIS-OBJECT:LoginCompanyShort = phBuffer::LoginCompanyShort .

        ASSIGN THIS-OBJECT:LoginCompanyTenantDomain = phBuffer::LoginCompanyTenantDomain .



    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Copies the entity table's properties to the buffer values                                                                         
        Notes:                      
        @param phBuffer The handle of the record buffer to assign properties from                                                             
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID AssignToBuffer (phBuffer AS HANDLE):
    
        ASSIGN phBuffer::LoginCompanyGuid = THIS-OBJECT:LoginCompanyGuid .

        ASSIGN phBuffer::LoginCompanyImageLarge = THIS-OBJECT:LoginCompanyImageLarge .

        ASSIGN phBuffer::LoginCompanyImageSmall = THIS-OBJECT:LoginCompanyImageSmall .

        ASSIGN phBuffer::LoginCompanyName = THIS-OBJECT:LoginCompanyName .

        ASSIGN phBuffer::LoginCompanyReferenceChar = THIS-OBJECT:LoginCompanyReferenceChar .

        ASSIGN phBuffer::LoginCompanyReferenceDecimal = THIS-OBJECT:LoginCompanyReferenceDecimal .

        ASSIGN phBuffer::LoginCompanyReferenceInteger = THIS-OBJECT:LoginCompanyReferenceInteger .

        ASSIGN phBuffer::LoginCompanyShort = THIS-OBJECT:LoginCompanyShort .

        ASSIGN phBuffer::LoginCompanyTenantDomain = THIS-OBJECT:LoginCompanyTenantDomain .

    
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the SmartLoginCompany_Generated class                                                                      
        Notes:                                                                        
        @param phBuffer The handle of the record buffer to assign fields to 
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartLoginCompany_Generated ():

    END DESTRUCTOR.

END CLASS.
