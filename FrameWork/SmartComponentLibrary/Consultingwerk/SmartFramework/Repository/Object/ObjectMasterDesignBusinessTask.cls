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
    File        : ObjectMasterDesignBusinessTask
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 25 18:50:13 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                        FROM PROPATH .
USING Consultingwerk.Framework.Collections.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.*  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.OERA.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Object.ObjectMasterDesignBusinessTask
    IMPLEMENTS IBusinessService:

    /*------------------------------------------------------------------------------
        Purpose: Returns the Design Time Data for the given object master
        Notes:
        @param phDataset The dataset with the design data to return to the caller
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectMasterDesignData (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                                  poParameter AS GetObjectMasterDesignDataParameter):

        DEFINE VARIABLE oObjectMaster             AS ObjectMasterDatasetModel     NO-UNDO .
        DEFINE VARIABLE oRepositoryCacheParameter AS CharacterList                NO-UNDO .
        DEFINE VARIABLE oCacheFactory             AS IRepositoryCacheTableFactory NO-UNDO .

        HandleAssert:InValidHandle (phDataset, "phDataset":U) .
        ObjectAssert:IsValid(poParameter, "poParameter":U) .

        oObjectMaster = NEW ObjectMasterDatasetModel () .
        oObjectMaster:SmartObjectMaster:Filter:ObjectName:EQ (poParameter:ObjectName):Run() .

        oRepositoryCacheParameter = NEW CharacterList ().
        oRepositoryCacheParameter:Add (oObjectMaster:SmartObjectMaster:ObjectTypeGuid) .

        oCacheFactory = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.Class.IRepositoryCacheTableFactory
                                                      "NEW Consultingwerk.SmartFramework.Repository.Class.RepositoryCacheTableFactory ()"} .

        phDataset = oCacheFactory:GetRepositoryCacheTables(GetAttributesForEnum:DesignTime, oRepositoryCacheParameter) .


        DELETE OBJECT phDataset .

    END METHOD.

END CLASS.
