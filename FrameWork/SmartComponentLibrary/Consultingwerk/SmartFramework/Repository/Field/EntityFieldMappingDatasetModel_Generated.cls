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
    File        : EntityFieldMapping_Generated
    Purpose     : Auto Generated portion of the EntityFieldMapping class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 12/11/2016 13:43:12,816+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Field.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Repository/Field/dsEntityFieldMapping.i &ACCESS=PROTECTED }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartEntityFieldMapping table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartEntityFieldMapping AS SmartEntityFieldMappingTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EntityFieldMappingDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EntityFieldMappingDatasetModel_Generated ():
        SUPER (DATASET dsEntityFieldMapping:HANDLE).

        THIS-OBJECT:SmartEntityFieldMapping = NEW SmartEntityFieldMappingTableModel (THIS-OBJECT, BUFFER eSmartEntityFieldMapping:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartEntityFieldMapping) .

    
    END CONSTRUCTOR.

  

END CLASS.
