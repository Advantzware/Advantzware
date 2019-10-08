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
    File        : SmartLoginCompanyTableModel_Generated
    Purpose     : Auto Generated portion of the SmartLoginCompanyTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:28,820+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.LoginCompanyBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.SmartLoginCompanyTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LoginCompanyGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LoginCompanyGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LoginCompanyName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LoginCompanyName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyShort value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyShort AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LoginCompanyShort":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LoginCompanyShort":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyImageLarge value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyImageLarge AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LoginCompanyImageLarge":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LoginCompanyImageLarge":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyImageSmall value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyImageSmall AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LoginCompanyImageSmall":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LoginCompanyImageSmall":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyReferenceChar value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyReferenceChar AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LoginCompanyReferenceChar":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LoginCompanyReferenceChar":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyReferenceDecimal value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyReferenceDecimal AS DECIMAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetDecimalFieldValue ("LoginCompanyReferenceDecimal":U) .
    END GET . 
    PUBLIC SET (arg AS DECIMAL):
        THIS-OBJECT:SetDecimalFieldValue ("LoginCompanyReferenceDecimal":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyReferenceInteger value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyReferenceInteger AS INTEGER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetIntegerFieldValue ("LoginCompanyReferenceInteger":U) .
    END GET . 
    PUBLIC SET (arg AS INTEGER):
        THIS-OBJECT:SetIntegerFieldValue ("LoginCompanyReferenceInteger":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyTenantDomain value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyTenantDomain AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LoginCompanyTenantDomain":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LoginCompanyTenantDomain":U, arg) .
    END SET .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartLoginCompanyTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS SmartLoginCompanyTableModelFilter NO-UNDO.
        
        oFilter = NEW SmartLoginCompanyTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartLoginCompanyTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartLoginCompanyTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartLoginCompanyTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartLoginCompanyTableModel on which this SmartLoginCompanyTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartLoginCompanyTableModel_Generated (poTableModel AS SmartLoginCompanyTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartLoginCompanyTableModel as a view model for the current record
       Notes:
       @return The SmartLoginCompanyTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartLoginCompanyTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     SmartLoginCompanyTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartLoginCompanyTableModel as a view model
       Notes:
       @return The SmartLoginCompanyTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartLoginCompanyTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     SmartLoginCompanyTableModel) . 
        
    END METHOD.
    
END CLASS.
