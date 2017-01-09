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
    File        : MenuStyleBusinessEntity
    Purpose     : Business Entity for MenuStyle
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 09.08.2013 08:55:19
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .
USING Consultingwerk.Util.*                FROM PROPATH .
USING Progress.Lang.*                      FROM PROPATH .

@BusinessEntityTable (name="eSmartMenuStyle", MandatoryColumns="MenuStyleCode", DefaultSort="BY eSmartMenuStyle.MenuStyleCode") .

CLASS Consultingwerk.SmartFramework.Menu.MenuStyleBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/Menu/dsMenuStyle.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuStyleBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuStyleBusinessEntity ():
        SUPER (DATASET dsMenuStyle:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Menu.MenuStyleDataAccess":U .

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

    END METHOD.

END CLASS.
