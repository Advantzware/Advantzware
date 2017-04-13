/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartBusinessEntityBusinessEntity
    Purpose     : Business Entity for SmartBusinessEntity
    Syntax      :
    Description :
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : 17.10.2013 19:02:05
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                        FROM PROPATH .
USING Consultingwerk.SmartFramework.*              FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*       FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.* FROM PROPATH.
USING Consultingwerk.Util.*                        FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

@BusinessEntityTable (name="eSmartBusinessEntity", MandatoryColumns="BusinessEntityPackage,BusinessEntityName", DefaultSort="BY eSmartBusinessEntity.BusinessEntityPackage BY eSmartBusinessEntity.BusinessEntityName") .
@BusinessEntityTable (name="eSmartBusinessEntityTable", MandatoryColumns="", DefaultSort="BY eSmartBusinessEntityTable.TableOrder") .

CLASS Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/System/dsSmartBusinessEntity.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartBusinessEntityBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityBusinessEntity ():
        SUPER (DATASET dsSmartBusinessEntity:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.SmartBusinessEntityDataAccess":U .

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

        DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cBusinessEntity AS CHARACTER NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 09.05.2016
           Ignore validation while importing data */
        IF GenericDataImporter:Importing THEN
            RETURN .

        FOR EACH eSmartBusinessEntity ON ERROR UNDO, THROW:

            ASSIGN cBusinessEntity = SUBSTITUTE ("&1.&2":U,
                                                 eSmartBusinessEntity.BusinessEntityPackage,
                                                 eSmartBusinessEntity.BusinessEntityName).

            IF NOT ServiceInterface:ValidateBusinessEntityName (cBusinessEntity) THEN DO:
                ASSIGN cMessage = MessageFormatter:GetMessage ("VALMSG":U, 2, "Business Entity Name":U,
                                                               cBusinessEntity).

                Consultingwerk.Util.DatasetHelper:AddErrorString (BUFFER eSmartBusinessEntity:HANDLE, cMessage) .
            END.

        END.

        FOR EACH eSmartBusinessEntityTable ON ERROR UNDO, THROW:

            ASSIGN cMessage = MessageFormatter:GetMessage ("VALMSG":U, 2, "TableOrder":U,
                                                           STRING (eSmartBusinessEntityTable.TableOrder)).

            Consultingwerk.OERA.Validate:IsNotUnknownOrZero (BUFFER eSmartBusinessEntityTable:HANDLE, "TableOrder":U, cMessage).

        END.

    END METHOD.

END CLASS.
