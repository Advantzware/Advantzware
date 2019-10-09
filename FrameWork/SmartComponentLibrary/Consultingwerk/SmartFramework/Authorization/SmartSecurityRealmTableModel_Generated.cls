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
    File        : SmartSecurityRealmTableModel_Generated
    Purpose     : Auto Generated portion of the SmartSecurityRealmTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:19,445+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authorization.SecurityRealmBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.Authorization.SmartSecurityRealmTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityRealmGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityRealmGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityRealmGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityRealmGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityRealmCode value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityRealmCode AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityRealmCode":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityRealmCode":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityRealmDescription value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityRealmDescription AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityRealmDescription":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityRealmDescription":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityRealmService value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityRealmService AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityRealmService":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityRealmService":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current DefaultRestricted value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DefaultRestricted AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("DefaultRestricted":U) .
    END GET . 
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("DefaultRestricted":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current CompanyDependent value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CompanyDependent AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("CompanyDependent":U) .
    END GET . 
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("CompanyDependent":U, arg) .
    END SET .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartSecurityRealmTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS SmartSecurityRealmTableModelFilter NO-UNDO.
        
        oFilter = NEW SmartSecurityRealmTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSecurityRealmTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSecurityRealmTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSecurityRealmTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartSecurityRealmTableModel on which this SmartSecurityRealmTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartSecurityRealmTableModel_Generated (poTableModel AS SmartSecurityRealmTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartSecurityRealmTableModel as a view model for the current record
       Notes:
       @return The SmartSecurityRealmTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartSecurityRealmTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     SmartSecurityRealmTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartSecurityRealmTableModel as a view model
       Notes:
       @return The SmartSecurityRealmTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartSecurityRealmTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     SmartSecurityRealmTableModel) . 
        
    END METHOD.
    
END CLASS.
