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
    File        : ToolbarSecurityItemBusinessEntity
    Purpose     : Business Entity for SmartToolbarSecurityItem
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20.04.2014 11:44:41
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

@BusinessEntityTable (name="eSmartToolbarSecurityItem", MandatoryColumns="SecurityTokenCode", DefaultSort="BY eSmartToolbarSecurityItem.ObjectName BY eSmartToolbarSecurityItem.ToolbarItemKey") .

CLASS Consultingwerk.SmartFramework.Authorization.ToolbarSecurityItemBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/Authorization/dsToolbarSecurityItem.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ToolbarSecurityItemBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ToolbarSecurityItemBusinessEntity ():
        SUPER (DATASET dsToolbarSecurityItem:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Authorization.ToolbarSecurityItemDataAccess":U .

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

        FOR EACH eSmartToolbarSecurityItem ON ERROR UNDO, THROW:

            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartToolbarSecurityItem:HANDLE,
                                                           "ToolbarItemKey":U,
                                                           MessageFormatter:GetMessage ("VALMSG":U, 1, "Toolbar Item Key"{&TRAN})) .
        END.

    END METHOD.

END CLASS.
