/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartModuleTableModel_Generated
    Purpose     : Auto Generated portion of the SmartModuleTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:24,343+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.Menu.SmartModuleTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ModuleGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ModuleGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ModuleGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ModuleGuid":U, arg) .
    END SET .

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
        Purpose: Gets and sets the current ModuleDescription value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ModuleDescription AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ModuleDescription":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ModuleDescription":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ProductGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ProductGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ProductGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ProductGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ProductCode value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ProductCode AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ProductCode":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ProductCode":U, arg) .
    END SET .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartModuleTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS SmartModuleTableModelFilter NO-UNDO.
        
        oFilter = NEW SmartModuleTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartModuleTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartModuleTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartModuleTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartModuleTableModel on which this SmartModuleTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartModuleTableModel_Generated (poTableModel AS SmartModuleTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartModuleTableModel as a view model for the current record
       Notes:
       @return The SmartModuleTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartModuleTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     SmartModuleTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartModuleTableModel as a view model
       Notes:
       @return The SmartModuleTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartModuleTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     SmartModuleTableModel) . 
        
    END METHOD.
    
END CLASS.
