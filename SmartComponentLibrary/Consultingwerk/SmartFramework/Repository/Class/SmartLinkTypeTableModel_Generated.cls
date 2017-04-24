/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartLinkTypeTableModel_Generated
    Purpose     : Auto Generated portion of the SmartLinkTypeTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03/05/2016 23:33:54,099+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.LinkTypeBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.Repository.Class.SmartLinkTypeTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LinkTypeGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkTypeGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LinkTypeGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LinkTypeGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the LinkTypeGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER LinkTypeGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("LinkTypeGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LinkName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LinkName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LinkName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the LinkName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER LinkNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("LinkName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SourcePropertyName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SourcePropertyName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SourcePropertyName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SourcePropertyName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the SourcePropertyName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER SourcePropertyNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("SourcePropertyName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SourceHandshakeMethod value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SourceHandshakeMethod AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SourceHandshakeMethod":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SourceHandshakeMethod":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the SourceHandshakeMethod field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER SourceHandshakeMethodWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("SourceHandshakeMethod":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current TargetPropertyName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TargetPropertyName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("TargetPropertyName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("TargetPropertyName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the TargetPropertyName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER TargetPropertyNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("TargetPropertyName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current TargetHandshakeMethod value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TargetHandshakeMethod AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("TargetHandshakeMethod":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("TargetHandshakeMethod":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the TargetHandshakeMethod field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER TargetHandshakeMethodWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("TargetHandshakeMethod":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartLinkTypeTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS SmartLinkTypeTableModelFilter NO-UNDO.
        
        oFilter = NEW SmartLinkTypeTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartLinkTypeTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartLinkTypeTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartLinkTypeTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartLinkTypeTableModel on which this SmartLinkTypeTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartLinkTypeTableModel_Generated (poTableModel AS SmartLinkTypeTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartLinkTypeTableModel as a view model for the current record
       Notes:
       @return The SmartLinkTypeTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartLinkTypeTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     SmartLinkTypeTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartLinkTypeTableModel as a view model
       Notes:
       @return The SmartLinkTypeTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartLinkTypeTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     SmartLinkTypeTableModel) . 
        
    END METHOD.
    
END CLASS.
