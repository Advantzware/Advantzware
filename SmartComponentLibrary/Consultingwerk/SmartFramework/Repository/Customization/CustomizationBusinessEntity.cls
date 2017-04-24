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
    File        : CustomizationBusinessEntity
    Purpose     : Business Entity for Customization
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 09.03.2016 07:32:10
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                               FROM PROPATH .
USING Consultingwerk.SmartFramework.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Customization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.*             FROM PROPATH .
USING Consultingwerk.OERA.*                                    FROM PROPATH .
USING Consultingwerk.Util.*                                    FROM PROPATH .
USING Progress.Lang.*                                          FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Customization.CustomizationBusinessEntity":U, type="BusinessEntity":U) .

CLASS Consultingwerk.SmartFramework.Repository.Customization.CustomizationBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Customization/dsCustomization.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CustomizationBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CustomizationBusinessEntity ():
        SUPER (DATASET dsCustomization:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Customization.CustomizationDataAccess":U .

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

        DEFINE VARIABLE cCustomizationBEName AS CHARACTER NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 09.05.2016
           Ignore validation while importing data */
        IF GenericDataImporter:Importing THEN
            RETURN .

        cCustomizationBEName = "Consultingwerk.SmartFramework.Repository.Customization.CustomizationBusinessEntity":U + CHR (1) + GUID .

        FOR EACH eSmartCustomizationResult ON ERROR UNDO, THROW:

            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartCustomizationResult:HANDLE,
                                                           "CustomizationResultCode":U,
                                                           MessageFormatter:GetMessage ("VALMSG":U, 1, "Customization Result Code":U)).

            IF Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartCustomizationResult:HANDLE,
                                                              "CustomizationTypeGuid":U,
                                                              MessageFormatter:GetMessage ("VALMSG":U, 1, "Customization Type":U)) THEN DO:

                Consultingwerk.OERA.Validate:CanFind (BUFFER eSmartCustomizationResult:HANDLE,
                                                      "CustomizationTypeGuid":U,
                                                      "Consultingwerk.SmartFramework.Repository.Customization.CustomizationTypeBusinessEntity":U,
                                                      "eSmartCustomizationType":U,
                                                      SUBSTITUTE ("eSmartCustomizationType.CustomizationTypeGuid = &1":U, QUOTER (eSmartCustomizationResult.CustomizationTypeGuid)),
                                                      MessageFormatter:GetMessage ("VALMSG":U, 2, "Customization Type":U)).
            END.

        END.

        FOR EACH eSmartCustomization ON ERROR UNDO, THROW:

            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartCustomization:HANDLE,
                                                           "ReferenceValue":U,
                                                           MessageFormatter:GetMessage ("VALMSG":U, 1, "Reference Value":U)).

            IF Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartCustomization:HANDLE,
                                                              "CustomizationTypeGuid":U,
                                                              MessageFormatter:GetMessage ("VALMSG":U, 1, "Customization Type":U)) THEN DO:

                Consultingwerk.OERA.Validate:CanFind (BUFFER eSmartCustomization:HANDLE,
                                                      "CustomizationTypeGuid":U,
                                                      "Consultingwerk.SmartFramework.Repository.Customization.CustomizationTypeBusinessEntity":U,
                                                      "eSmartCustomizationType":U,
                                                      SUBSTITUTE ("eSmartCustomizationType.CustomizationTypeGuid = &1":U, QUOTER (eSmartCustomization.CustomizationTypeGuid)),
                                                      MessageFormatter:GetMessage ("VALMSG":U, 2, "Customization Type":U)).
            END.

            IF Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartCustomization:HANDLE,
                                                              "CustomizationResultGuid":U,
                                                              MessageFormatter:GetMessage ("VALMSG":U, 1, "Customization Result":U)) THEN DO:

                Consultingwerk.OERA.Validate:CanFind (BUFFER eSmartCustomization:HANDLE,
                                                      "CustomizationResultGuid":U,
                                                      cCustomizationBEName,
                                                      "eSmartCustomizationResult":U,
                                                      SUBSTITUTE ("eSmartCustomizationResult.CustomizationResultGuid = &1":U, QUOTER (eSmartCustomization.CustomizationResultGuid)),
                                                      MessageFormatter:GetMessage ("VALMSG":U, 2, "Customization Result":U)).
            END.

        END.

        FINALLY:
            IF ServiceInterface:ValidateBusinessEntityName (cCustomizationBEName) THEN
                ServiceInterface:StopBusinessEntity (cCustomizationBEName).
        END FINALLY.
    END METHOD.

END CLASS.
