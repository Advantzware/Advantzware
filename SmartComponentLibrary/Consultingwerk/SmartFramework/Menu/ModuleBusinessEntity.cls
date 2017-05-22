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
    File        : ModuleBusinessEntity
    Purpose     : Business Entity for Module
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories
    Created     : 11.10.2012 17:14:27
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.BusinessEntity   FROM PROPATH .
USING Consultingwerk.Util.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .
USING Progress.Lang.*                      FROM PROPATH .

@BusinessEntityTable (name="eSmartModule", MandatoryColumns="ModuleName", DefaultSort="BY eSmartModule.ModuleName") .

CLASS Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/Menu/dsModule.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ModuleBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModuleBusinessEntity ():
        SUPER (DATASET dsModule:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Menu.ModuleDataAccess":U .

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

        FOR EACH eSmartModule ON ERROR UNDO, THROW:

            IF eSmartModule.ModuleDataFolder > "":U THEN DO:

                IF LENGTH (eSmartModule.ModuleDataFolder, "CHARACTER":U) > 2 AND
                    SUBSTRING (eSmartModule.ModuleDataFolder, 2, 1, "CHARACTER":U) = ":":U OR

                   eSmartModule.ModuleDataFolder BEGINS "//":U OR
                   eSmartModule.ModuleDataFolder BEGINS "~\~\":U OR
                   eSmartModule.ModuleDataFolder BEGINS ".":U THEN

                    DatasetHelper:AddErrorString (BUFFER eSmartModule:HANDLE,
                                                  MessageFormatter:GetMessage("SFR":U, 403),
                                                  "ModuleDataFolder":U) .
            END.
        END.

    END METHOD.

END CLASS.
