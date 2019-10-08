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
    File        : IRepositoryCacheTableFactory
    Purpose     : Interface for services that generate the repository cache
                  temp-tables
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 25 22:32:41 CET 2016
    Notes       : Only returns attributes with the AppliesAtRuntime flag
                  set to true
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Collections.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.Repository.Class.IRepositoryCacheTableFactory:

    /*------------------------------------------------------------------------------
        Purpose: Returns a dataset handle that contains the cache temp-tables for the
                 given object types
        Notes:   The caller is responsible for deleting the returned Dataset
        @param poAttributesFrom The GetAttributesForEnum indicating if attributes should be added for runtime or design
        @param poParameter The CharacterList with the names or guids of the object types for this call
        @return The dataset handle that contains the cache temp-tables for the given object types
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetRepositoryCacheTables (poAttributesFrom AS GetAttributesForEnum,
                                                   poParameter AS CharacterList).

    /*------------------------------------------------------------------------------
        Purpose: Returns a dataset handle that contains the cache temp-tables for the
                 given object types
        Notes:   The caller is responsible for deleting the returned Dataset
        @param poAttributesFrom The GetAttributesForEnum indicating if attributes should be added for runtime or design
        @param poParameter The CharacterList with the names or guids of the object types for this call
        @param phDataset The dataset handle to add the cache temp-tables for the given object types to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetRepositoryCacheTables (poAttributesFrom AS GetAttributesForEnum,
                                                 poParameter AS CharacterList,
                                                 phDataset AS HANDLE) .

END INTERFACE.
