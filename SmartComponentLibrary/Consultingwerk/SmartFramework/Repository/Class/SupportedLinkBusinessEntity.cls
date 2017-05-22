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
    File        : SupportedLinkBusinessEntity
    Purpose     : Business Entity for SupportedLink
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.05.2016 23:06:50
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.*     FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

{Consultingwerk/products.i}

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.SupportedLinkBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Repository.Class.SupportedLinkBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Class/dsSupportedLink.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SupportedLinkBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SupportedLinkBusinessEntity ():
        SUPER (DATASET dsSupportedLink:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Class.SupportedLinkDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

        DEFINE VARIABLE oSmartLinkType AS LinkTypeDatasetModel NO-UNDO .

        FOR EACH eSmartSupportedLink BREAK BY eSmartSupportedLink.LinkTypeGuid ON ERROR UNDO, THROW:

            IF FIRST-OF (eSmartSupportedLink.LinkTypeGuid) THEN
                oSmartLinkType = NEW LinkTypeDatasetModel(eSmartSupportedLink.LinkTypeGuid) .

            eSmartSupportedLink.LinkName = oSmartLinkType:SmartLinkType:LinkNameWhenAvailable("":U).
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

        /* Mike Fechner, Consultingwerk Ltd. 09.05.2016
           Ignore validation while importing data */
        IF GenericDataImporter:Importing THEN
            RETURN .

        FOR EACH eSmartSupportedLink ON ERROR UNDO, THROW:

            ValidationHelper:IsNotNullOrEmpty (BUFFER eSmartSupportedLink:HANDLE,
                                               "LinkTypeGuid":U,
                                               "Please enter Link Type"{&TRAN}) .

            ValidationHelper:CanFind (BUFFER eSmartSupportedLink:HANDLE,
                                      "LinkTypeGuid":U,
                                      "Consultingwerk.SmartFramework.Repository.Class.LinkTypeBusinessEntity":U,
                                      "eSmartLinkType":U,
                                      "for each eSmartLinkType where eSmartLinkType.LinkTypeGuid = &1":U,
                                      "Link Type is invalid"{&TRAN}) .
        END.

    END METHOD.

END CLASS.
