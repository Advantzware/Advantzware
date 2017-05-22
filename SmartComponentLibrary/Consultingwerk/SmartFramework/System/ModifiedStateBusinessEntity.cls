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
    File        : ModifiedStateBusinessEntity
    Purpose     : Business Entity for ModifiedState
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31.12.2016 00:59:16
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Assertion.*             FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

/* Sample Annotations - see http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
@BusinessEntityView (name="order", isdefault="true", entitytable="eOrder", entityview="eCustomer",
                     listcolumns="eOrder.OrderNum,eOrder.OrderDate,eOrder.CustNum,eCustomer.Name",
                     viewercolumns="eOrder.OrderNum,eOrder.OrderDate,eOrder.CustNum,eCustomer.Name,eOrder.OrderStatus") .
@BusinessEntityTable (name="eOrder", mandatoryColumns="ordernum,orderstatus", readonlyColumns="ordernum") .
@BusinessEntityTable (name="eCustomer", mandatoryColumns="ordernum,orderstatus", readonly="true") .
*/

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.ModifiedStateBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.System.ModifiedStateBusinessEntity
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/System/dsModifiedState.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ModifiedStateBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModifiedStateBusinessEntity ():
        SUPER (DATASET dsModifiedState:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.ModifiedStateDataAccess":U .

    END CONSTRUCTOR.

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.JsonSerializable", datasetInput="false", datasetOutput="false") .
    /**
     * Purpose: Deletes all records
     * Notes:
     * @param dsModifiedState INPUT-OUTPUT DATASET
     * @param poParameter The Parameter Object for this method
     */
    METHOD PUBLIC VOID DeleteAll (INPUT-OUTPUT DATASET dsModifiedState, poParameter AS Consultingwerk.ISerializable):

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject() .

        CAST (THIS-OBJECT:DataAccessObject, ModifiedStateDataAccess):DeleteAll() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

        DEFINE VARIABLE oTableInfoProvider AS ITableInfoProvider NO-UNDO .
        DEFINE VARIABLE hBuffer            AS HANDLE             NO-UNDO .

        oTableInfoProvider  = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITableInfoProvider} .

        IF NOT VALID-OBJECT (oTableInfoProvider) THEN
            RETURN .

        FOR EACH eSmartModifiedState ON ERROR UNDO, THROW:

            CREATE BUFFER hBuffer FOR TABLE eSmartModifiedState.ModifiedStateTable .

            oTableInfoProvider:FindRecordByKeyValues (hBuffer,
                                                      eSmartModifiedState.ModifiedStateKeyFieldValues,
                                                      FALSE) .

            IF hBuffer:AVAILABLE THEN
                eSmartModifiedState.Description = oTableInfoProvider:RecordDescription (hBuffer) .
            ELSE
                eSmartModifiedState.Description = ? .

            CATCH err AS Progress.Lang.Error :
                eSmartModifiedState.Description = err:GetMessage(1) .
            END CATCH.
            FINALLY:
                GarbageCollectorHelper:DeleteObject (hBuffer).
            END FINALLY.
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

    END METHOD.

END CLASS.
