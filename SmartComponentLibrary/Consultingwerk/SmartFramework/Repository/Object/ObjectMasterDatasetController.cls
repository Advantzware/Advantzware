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
    File        : ObjectMasterDatasetController
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 15.05.2016 23:17:23
    Notes       : Conditionally implements the IDatasetController Interface
                  when the SmartComponentLibrary preprocessor option is set.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.* FROM PROPATH.
USING Consultingwerk.OERA.* FROM PROPATH.
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity", type="DatasetController") .

CLASS Consultingwerk.SmartFramework.Repository.Object.ObjectMasterDatasetController
    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN IMPLEMENTS IDatasetController &ENDIF :

    { Consultingwerk/SmartFramework/Repository/Object/dsObjectMaster.i }
    { Consultingwerk/SmartFramework/Repository/Class/dsAttribute.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the static dataset defined inside the
                 Dataset Controller
        Notes:   Interface IDatasetController member
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ObjectMasterDatasetController class
        Notes:   Assigns the DatasetHandle property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectMasterDatasetController ():
        SUPER ().

        THIS-OBJECT:DatasetHandle = DATASET dsObjectMaster:HANDLE .

        FrameworkSettings:ServiceAdapter:RetrieveData("":U,
                                                      "Consultingwerk.SmartFramework.Repository.Class.AttributeBusinessEntity":U,
                                                      NEW FetchDataRequest ("*":U),
                                                      OUTPUT DATASET dsAttribute) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when detaching from the Dataset of the DatasetController.
                 Allows event unsubscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object detaching from the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when attaching to the Dataset of the DatasetController.
                 Allows event subscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object attaching to the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

END CLASS.
