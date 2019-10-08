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
    File        : SmartKeyFieldAssignmentTypeTableModel_Generated
    Purpose     : Auto Generated portion of the SmartKeyFieldAssignmentTypeTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03/10/2015 10:32:39,106+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.SmartKeyFieldAssignmentTypeTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current KeyFieldAssignmentGUID value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY KeyFieldAssignmentGUID AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("KeyFieldAssignmentGUID":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("KeyFieldAssignmentGUID":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the KeyFieldAssignmentGUID field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER KeyFieldAssignmentGUIDWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("KeyFieldAssignmentGUID":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current KeyFieldAssignmentKey value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY KeyFieldAssignmentKey AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("KeyFieldAssignmentKey":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("KeyFieldAssignmentKey":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the KeyFieldAssignmentKey field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER KeyFieldAssignmentKeyWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("KeyFieldAssignmentKey":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current KeyFieldAssignmentDescription value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY KeyFieldAssignmentDescription AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("KeyFieldAssignmentDescription":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("KeyFieldAssignmentDescription":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the KeyFieldAssignmentDescription field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER KeyFieldAssignmentDescriptionWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:Available THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("KeyFieldAssignmentDescription":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartKeyFieldAssignmentTypeTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS SmartKeyFieldAssignmentTypeTableModelFilter NO-UNDO.
        
        oFilter = NEW SmartKeyFieldAssignmentTypeTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartKeyFieldAssignmentTypeTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartKeyFieldAssignmentTypeTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartKeyFieldAssignmentTypeTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartKeyFieldAssignmentTypeTableModel on which this SmartKeyFieldAssignmentTypeTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartKeyFieldAssignmentTypeTableModel_Generated (poTableModel AS SmartKeyFieldAssignmentTypeTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartKeyFieldAssignmentTypeTableModel as a view model for the current record
       Notes:
       @return The SmartKeyFieldAssignmentTypeTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartKeyFieldAssignmentTypeTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     SmartKeyFieldAssignmentTypeTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartKeyFieldAssignmentTypeTableModel as a view model
       Notes:
       @return The SmartKeyFieldAssignmentTypeTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartKeyFieldAssignmentTypeTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     SmartKeyFieldAssignmentTypeTableModel) . 
        
    END METHOD.
    
END CLASS.
