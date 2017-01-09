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
    File        : AttributeGroupBusinessEntity
    Purpose     : Business Entity for AttributeGroup
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 20.05.2014 17:41:22
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.*     FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

@BusinessEntityTable (name="eSmartAttributeGroup", MandatoryColumns="AttributeGroupName", DefaultSort="BY eSmartAttributeGroup.AttributeGroupName") .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeGroupBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/Repository/Class/dsAttributeGroup.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AttributeGroupBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeGroupBusinessEntity ():
        SUPER (DATASET dsAttributeGroup:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Class.AttributeGroupDataAccess":U .

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

    END METHOD.

END CLASS.
