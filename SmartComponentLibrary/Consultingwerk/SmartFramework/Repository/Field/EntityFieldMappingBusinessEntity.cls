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
    File        : EntityFieldMappingBusinessEntity
    Purpose     : Business Entity for EntityFieldMapping
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.11.2016 08:47:47
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Field.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.*     FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.ReposiFieldField.EntityFieldMappingBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Field/dsEntityFieldMapping.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the EntityFieldMappingBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EntityFieldMappingBusinessEntity ():
        SUPER (DATASET dsEntityFieldMapping:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

        /* Mike Fechner, Consultingwerk Ltd. 09.05.2016
           Ignore validation while importing data */
        IF GenericDataImporter:Importing THEN
            RETURN .

        @GeneratedValidationStart.
        /* The following code is auto-generated. Don't insert custom code
           between the @GeneratedValidationStart. and @GeneratedValidationEnd.
           annotations as it would get overwritten when you update the Business
           entity. */

        FOR EACH eSmartEntityFieldMapping ON ERROR UNDO, THROW:
            Consultingwerk.OERA.ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartEntityFieldMapping:HANDLE, "EntityName":U, Consultingwerk.SmartFramework.MessageFormatter:GetMessage ("VALMSG":U, 1, "EntityName":U, eSmartEntityFieldMapping.EntityName)).
            Consultingwerk.OERA.ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartEntityFieldMapping:HANDLE, "EntityTable":U, Consultingwerk.SmartFramework.MessageFormatter:GetMessage ("VALMSG":U, 1, "EntityTable":U, eSmartEntityFieldMapping.EntityTable)).
            Consultingwerk.OERA.ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartEntityFieldMapping:HANDLE, "FieldName":U, Consultingwerk.SmartFramework.MessageFormatter:GetMessage ("VALMSG":U, 1, "FieldName":U, eSmartEntityFieldMapping.FieldName)).
            Consultingwerk.OERA.ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartEntityFieldMapping:HANDLE, "FieldObjectName":U, Consultingwerk.SmartFramework.MessageFormatter:GetMessage ("VALMSG":U, 1, "FieldObjectName":U, eSmartEntityFieldMapping.FieldObjectName)).
        END .
        @GeneratedValidationEnd.

    END METHOD.

END CLASS.
