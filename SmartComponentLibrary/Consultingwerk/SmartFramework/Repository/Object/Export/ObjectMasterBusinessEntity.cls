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
    File        : ObjectMasterBusinessEntity
    Purpose     : Business Entity for Exporting and Importing Object Master
                  data to .smartrepo fields
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31.12.2016 15:55:44
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                         FROM PROPATH .
USING Consultingwerk.Assertion.*                               FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.Export.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*                          FROM PROPATH .
USING Consultingwerk.OERA.*                                    FROM PROPATH .
USING Consultingwerk.Util.*                                    FROM PROPATH .
USING Progress.Lang.*                                          FROM PROPATH .

/* Sample Annotations - see http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
@BusinessEntityView (name="order", isdefault="true", entitytable="eOrder", entityview="eCustomer",
                     listcolumns="eOrder.OrderNum,eOrder.OrderDate,eOrder.CustNum,eCustomer.Name",
                     viewercolumns="eOrder.OrderNum,eOrder.OrderDate,eOrder.CustNum,eCustomer.Name,eOrder.OrderStatus") .
@BusinessEntityTable (name="eOrder", mandatoryColumns="ordernum,orderstatus", readonlyColumns="ordernum") .
@BusinessEntityTable (name="eCustomer", mandatoryColumns="ordernum,orderstatus", readonly="true") .
*/

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Object/Export/dsObjectMaster.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ObjectMasterBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectMasterBusinessEntity ():
        SUPER (DATASET dsObjectMaster:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterDataAccess":U .

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
