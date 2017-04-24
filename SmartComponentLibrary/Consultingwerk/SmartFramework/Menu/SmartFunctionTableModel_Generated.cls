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
    File        : SmartFunctionTableModel_Generated
    Purpose     : Auto Generated portion of the SmartFunctionTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 15/12/2015 19:13:29,417+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Menu.MenuBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.Menu.SmartFunctionTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current FunctionGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FunctionGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("FunctionGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("FunctionGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the FunctionGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER FunctionGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("FunctionGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current FunctionName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FunctionName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("FunctionName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("FunctionName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the FunctionName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER FunctionNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("FunctionName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current FunctionDescription value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FunctionDescription AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("FunctionDescription":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("FunctionDescription":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the FunctionDescription field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER FunctionDescriptionWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("FunctionDescription":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current FunctionSmallImage value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FunctionSmallImage AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("FunctionSmallImage":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("FunctionSmallImage":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the FunctionSmallImage field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER FunctionSmallImageWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("FunctionSmallImage":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current FunctionLargeImage value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FunctionLargeImage AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("FunctionLargeImage":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("FunctionLargeImage":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the FunctionLargeImage field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER FunctionLargeImageWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("FunctionLargeImage":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current FunctionCallParameter value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FunctionCallParameter AS LONGCHAR NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLongcharFieldValue ("FunctionCallParameter":U) .
    END GET . 
    PUBLIC SET (arg AS LONGCHAR):
        THIS-OBJECT:SetLongcharFieldValue ("FunctionCallParameter":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the FunctionCallParameter field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR FunctionCallParameterWhenAvailable (pDefaultValue AS LONGCHAR):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("FunctionCallParameter":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current FunctionModuleGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FunctionModuleGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("FunctionModuleGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("FunctionModuleGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the FunctionModuleGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER FunctionModuleGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("FunctionModuleGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ModuleName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ModuleName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ModuleName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ModuleName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ModuleName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ModuleNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ModuleName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ExternalId value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExternalId AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ExternalId":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ExternalId":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ExternalId field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ExternalIdWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ExternalId":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartFunctionTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS SmartFunctionTableModelFilter NO-UNDO.
        
        oFilter = NEW SmartFunctionTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartFunctionTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartFunctionTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartFunctionTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartFunctionTableModel on which this SmartFunctionTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartFunctionTableModel_Generated (poTableModel AS SmartFunctionTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartFunctionTableModel as a view model for the current record
       Notes:
       @return The SmartFunctionTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartFunctionTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     SmartFunctionTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartFunctionTableModel as a view model
       Notes:
       @return The SmartFunctionTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartFunctionTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     SmartFunctionTableModel) . 
        
    END METHOD.
    
END CLASS.
