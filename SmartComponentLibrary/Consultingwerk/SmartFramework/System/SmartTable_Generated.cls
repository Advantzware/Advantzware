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
    File        : SmartTable
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

CLASS Consultingwerk.SmartFramework.System.SmartTable_Generated 
    INHERITS EntityTable
    ABSTRACT: 

    DEFINE PUBLIC PROPERTY AllowAttachments AS LOGICAL  NO-UNDO
    GET.
    PUBLIC SET (arg as LOGICAL):
        THIS-OBJECT:AllowAttachments = arg .
        THIS-OBJECT:SetModified () .
        OnAllowAttachmentsChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the AllowAttachments property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AllowAttachmentsChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the AllowAttachmentsChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAllowAttachmentsChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:AllowAttachmentsChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY AllowComments AS LOGICAL  NO-UNDO
    GET.
    PUBLIC SET (arg as LOGICAL):
        THIS-OBJECT:AllowComments = arg .
        THIS-OBJECT:SetModified () .
        OnAllowCommentsChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the AllowComments property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AllowCommentsChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the AllowCommentsChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAllowCommentsChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:AllowCommentsChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY DatabaseName AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:DatabaseName = arg .
        THIS-OBJECT:SetModified () .
        OnDatabaseNameChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the DatabaseName property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DatabaseNameChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the DatabaseNameChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnDatabaseNameChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:DatabaseNameChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY DescriptionFields AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:DescriptionFields = arg .
        THIS-OBJECT:SetModified () .
        OnDescriptionFieldsChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the DescriptionFields property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DescriptionFieldsChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the DescriptionFieldsChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnDescriptionFieldsChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:DescriptionFieldsChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY DescriptionFormats AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:DescriptionFormats = arg .
        THIS-OBJECT:SetModified () .
        OnDescriptionFormatsChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the DescriptionFormats property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DescriptionFormatsChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the DescriptionFormatsChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnDescriptionFormatsChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:DescriptionFormatsChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY DescriptionSubstitute AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:DescriptionSubstitute = arg .
        THIS-OBJECT:SetModified () .
        OnDescriptionSubstituteChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the DescriptionSubstitute property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DescriptionSubstituteChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the DescriptionSubstituteChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnDescriptionSubstituteChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:DescriptionSubstituteChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY QualifiedTableName AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:QualifiedTableName = arg .
        THIS-OBJECT:SetModified () .
        OnQualifiedTableNameChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the QualifiedTableName property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT QualifiedTableNameChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the QualifiedTableNameChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnQualifiedTableNameChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:QualifiedTableNameChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY TableDescription AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:TableDescription = arg .
        THIS-OBJECT:SetModified () .
        OnTableDescriptionChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the TableDescription property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT TableDescriptionChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the TableDescriptionChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnTableDescriptionChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:TableDescriptionChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY TableGUID AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:TableGUID = arg .
        THIS-OBJECT:SetModified () .
        OnTableGUIDChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the TableGUID property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT TableGUIDChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the TableGUIDChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnTableGUIDChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:TableGUIDChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY TableName AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:TableName = arg .
        THIS-OBJECT:SetModified () .
        OnTableNameChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the TableName property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT TableNameChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the TableNameChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnTableNameChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:TableNameChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY TempTableName AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:TempTableName = arg .
        THIS-OBJECT:SetModified () .
        OnTempTableNameChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the TempTableName property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT TempTableNameChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the TempTableNameChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnTempTableNameChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:TempTableNameChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY UniqueKeyFields AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:UniqueKeyFields = arg .
        THIS-OBJECT:SetModified () .
        OnUniqueKeyFieldsChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the UniqueKeyFields property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT UniqueKeyFieldsChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the UniqueKeyFieldsChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnUniqueKeyFieldsChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:UniqueKeyFieldsChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY UniqueKeyFormats AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:UniqueKeyFormats = arg .
        THIS-OBJECT:SetModified () .
        OnUniqueKeyFormatsChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the UniqueKeyFormats property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT UniqueKeyFormatsChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the UniqueKeyFormatsChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnUniqueKeyFormatsChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:UniqueKeyFormatsChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .

    DEFINE PUBLIC PROPERTY UniqueKeySubstitute AS CHARACTER  NO-UNDO
    GET.
    PUBLIC SET (arg as CHARACTER):
        THIS-OBJECT:UniqueKeySubstitute = arg .
        THIS-OBJECT:SetModified () .
        OnUniqueKeySubstituteChanged (Consultingwerk.EventArgs:Empty) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the UniqueKeySubstitute property value changes
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT UniqueKeySubstituteChanged SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Raises the UniqueKeySubstituteChanged event
        Notes:           
        @param e The Consultingwerk.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnUniqueKeySubstituteChanged (e AS Consultingwerk.EventArgs):
        THIS-OBJECT:UniqueKeySubstituteChanged:Publish (THIS-OBJECT, e) . 
    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartTable_Generated class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTable_Generated ():
    
    
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartTable_Generated class                                                                      
        Notes:                                         
        @param phBuffer A source buffer to copy the values from                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTable_Generated (phBuffer AS HANDLE):
    
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

        ASSIGN THIS-OBJECT:AllowAttachments = phBuffer::AllowAttachments .

        ASSIGN THIS-OBJECT:AllowComments = phBuffer::AllowComments .

        ASSIGN THIS-OBJECT:DatabaseName = phBuffer::DatabaseName .

        ASSIGN THIS-OBJECT:DescriptionFields = phBuffer::DescriptionFields .

        ASSIGN THIS-OBJECT:DescriptionFormats = phBuffer::DescriptionFormats .

        ASSIGN THIS-OBJECT:DescriptionSubstitute = phBuffer::DescriptionSubstitute .

        ASSIGN THIS-OBJECT:QualifiedTableName = phBuffer::QualifiedTableName .

        ASSIGN THIS-OBJECT:TableDescription = phBuffer::TableDescription .

        ASSIGN THIS-OBJECT:TableGUID = phBuffer::TableGUID .

        ASSIGN THIS-OBJECT:TableName = phBuffer::TableName .

        ASSIGN THIS-OBJECT:TempTableName = phBuffer::TempTableName .

        ASSIGN THIS-OBJECT:UniqueKeyFields = phBuffer::UniqueKeyFields .

        ASSIGN THIS-OBJECT:UniqueKeyFormats = phBuffer::UniqueKeyFormats .

        ASSIGN THIS-OBJECT:UniqueKeySubstitute = phBuffer::UniqueKeySubstitute .



    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Copies the entity table's properties to the buffer values                                                                         
        Notes:                      
        @param phBuffer The handle of the record buffer to assign properties from                                                             
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID AssignToBuffer (phBuffer AS HANDLE):
    
        ASSIGN phBuffer::AllowAttachments = THIS-OBJECT:AllowAttachments .

        ASSIGN phBuffer::AllowComments = THIS-OBJECT:AllowComments .

        ASSIGN phBuffer::DatabaseName = THIS-OBJECT:DatabaseName .

        ASSIGN phBuffer::DescriptionFields = THIS-OBJECT:DescriptionFields .

        ASSIGN phBuffer::DescriptionFormats = THIS-OBJECT:DescriptionFormats .

        ASSIGN phBuffer::DescriptionSubstitute = THIS-OBJECT:DescriptionSubstitute .

        ASSIGN phBuffer::QualifiedTableName = THIS-OBJECT:QualifiedTableName .

        ASSIGN phBuffer::TableDescription = THIS-OBJECT:TableDescription .

        ASSIGN phBuffer::TableGUID = THIS-OBJECT:TableGUID .

        ASSIGN phBuffer::TableName = THIS-OBJECT:TableName .

        ASSIGN phBuffer::TempTableName = THIS-OBJECT:TempTableName .

        ASSIGN phBuffer::UniqueKeyFields = THIS-OBJECT:UniqueKeyFields .

        ASSIGN phBuffer::UniqueKeyFormats = THIS-OBJECT:UniqueKeyFormats .

        ASSIGN phBuffer::UniqueKeySubstitute = THIS-OBJECT:UniqueKeySubstitute .

    
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the SmartTable_Generated class                                                                      
        Notes:                                                                        
        @param phBuffer The handle of the record buffer to assign fields to 
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartTable_Generated ():

    END DESTRUCTOR.

END CLASS.
