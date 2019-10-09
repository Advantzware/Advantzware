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
    File        : TempTableTableModel_Generated
    Purpose     : Auto Generated portion of the TempTableTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:20,430+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Localization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Localization.StringResourcesBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.Localization.TempTableTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ResourceKey value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResourceKey AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ResourceKey":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ResourceKey":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ResourceKeyPartial value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResourceKeyPartial AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("ResourceKeyPartial":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("ResourceKeyPartial":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current OriginalString value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OriginalString AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("OriginalString":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("OriginalString":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current TranslatedString value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TranslatedString AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("TranslatedString":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("TranslatedString":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current TranslationType value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TranslationType AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("TranslationType":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("TranslationType":U, arg) .
    END SET .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS TempTableTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS TempTableTableModelFilter NO-UNDO.
        
        oFilter = NEW TempTableTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TempTableTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TempTableTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TempTableTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to TempTableTableModel on which this TempTableTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED TempTableTableModel_Generated (poTableModel AS TempTableTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the TempTableTableModel as a view model for the current record
       Notes:
       @return The TempTableTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TempTableTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     TempTableTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the TempTableTableModel as a view model
       Notes:
       @return The TempTableTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TempTableTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     TempTableTableModel) . 
        
    END METHOD.
    
END CLASS.
