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
    File        : FieldSecurityItemBusinessEntity
    Purpose     : Business Entity for SmartFieldSecurityItem
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 18.08.2014 10:56:39
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

{Consultingwerk/products.i}

@BusinessEntityTable (name="eSmartFieldSecurityItem", MandatoryColumns="FieldName", DefaultSort="BY eSmartFieldSecurityItem.FieldName BY eSmartFieldSecurityItem.ObjectName") .

CLASS Consultingwerk.SmartFramework.Authorization.FieldSecurityItemBusinessEntity INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/Authorization/dsFieldSecurityItem.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the FieldSecurityItemBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FieldSecurityItemBusinessEntity ():
        SUPER (DATASET dsFieldSecurityItem:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Authorization.FieldSecurityItemDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

        FOR EACH eSmartFieldSecurityItem ON ERROR UNDO, THROW:

            eSmartFieldSecurityItem.FieldNameCombined = eSmartFieldSecurityItem.FieldName +
                                                        (IF eSmartFieldSecurityItem.SecurityKey > "":U THEN " / ":U + eSmartFieldSecurityItem.SecurityKey ELSE "":U) +
                                                        (IF eSmartFieldSecurityItem.ObjectName > "":U  THEN " / ":U + eSmartFieldSecurityItem.ObjectName  ELSE "":U) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

        FOR EACH eSmartFieldSecurityItem ON ERROR UNDO, THROW:

            IF Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartFieldSecurityItem:HANDLE,
                                                              "FieldName":U,
                                                              MessageFormatter:GetMessage ("VALMSG":U, 1, "Field Name"{&TRAN})) = TRUE THEN

                IF NUM-ENTRIES (eSmartFieldSecurityItem.FieldName, ".":U) <> 2 THEN
                    DatasetHelper:AddErrorString (BUFFER eSmartFieldSecurityItem:HANDLE,
                                                  MessageFormatter:GetMessage ("SFR":U, 202),
                                                  "FieldName":U) .

        END.

    END METHOD.

END CLASS.
