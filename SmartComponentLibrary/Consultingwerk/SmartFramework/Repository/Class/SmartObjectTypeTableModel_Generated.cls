/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartObjectTypeTableModel_Generated
    Purpose     : Auto Generated portion of the SmartObjectTypeTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 16/10/2015 20:01:09,536+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.Repository.Class.SmartObjectTypeTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ObjectTypeGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ObjectTypeGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ObjectTypeGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ObjectTypeGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ObjectTypeGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ObjectTypeGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ObjectTypeGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ObjectTypeName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ObjectTypeName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ObjectTypeName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ObjectTypeName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ObjectTypeName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ObjectTypeNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ObjectTypeName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ObjectTypeDescription value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ObjectTypeDescription AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ObjectTypeDescription":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ObjectTypeDescription":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ObjectTypeDescription field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ObjectTypeDescriptionWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ObjectTypeDescription":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current DeploymentType value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DeploymentType AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("DeploymentType":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("DeploymentType":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the DeploymentType field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER DeploymentTypeWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("DeploymentType":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current StaticObject value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StaticObject AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("StaticObject":U) .
    END GET . 
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("StaticObject":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the StaticObject field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL StaticObjectWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("StaticObject":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ExtendsObjectTypeGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExtendsObjectTypeGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ExtendsObjectTypeGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ExtendsObjectTypeGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ExtendsObjectTypeGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ExtendsObjectTypeGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ExtendsObjectTypeGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ExtendsObjectTypeName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExtendsObjectTypeName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ExtendsObjectTypeName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ExtendsObjectTypeName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ExtendsObjectTypeName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ExtendsObjectTypeNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ExtendsObjectTypeName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current CustomObjectTypeGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CustomObjectTypeGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("CustomObjectTypeGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("CustomObjectTypeGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the CustomObjectTypeGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER CustomObjectTypeGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("CustomObjectTypeGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current CustomObjectTypeName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CustomObjectTypeName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("CustomObjectTypeName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("CustomObjectTypeName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the CustomObjectTypeName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER CustomObjectTypeNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("CustomObjectTypeName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current TechnicalClassName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TechnicalClassName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("TechnicalClassName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("TechnicalClassName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the TechnicalClassName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER TechnicalClassNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("TechnicalClassName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ClassTypeGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassTypeGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ClassTypeGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ClassTypeGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ClassTypeGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ClassTypeGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ClassTypeGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ClassTypeName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassTypeName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ClassTypeName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ClassTypeName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ClassTypeName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ClassTypeNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ClassTypeName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ContainerType value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContainerType AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("ContainerType":U) .
    END GET . 
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("ContainerType":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ContainerType field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ContainerTypeWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ContainerType":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current StoreInstances value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StoreInstances AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("StoreInstances":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("StoreInstances":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the StoreInstances field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER StoreInstancesWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("StoreInstances":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartObjectTypeTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS SmartObjectTypeTableModelFilter NO-UNDO.
        
        oFilter = NEW SmartObjectTypeTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartObjectTypeTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartObjectTypeTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartObjectTypeTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartObjectTypeTableModel on which this SmartObjectTypeTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartObjectTypeTableModel_Generated (poTableModel AS SmartObjectTypeTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartObjectTypeTableModel as a view model for the current record
       Notes:
       @return The SmartObjectTypeTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartObjectTypeTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     SmartObjectTypeTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartObjectTypeTableModel as a view model
       Notes:
       @return The SmartObjectTypeTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartObjectTypeTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     SmartObjectTypeTableModel) . 
        
    END METHOD.
    
END CLASS.
