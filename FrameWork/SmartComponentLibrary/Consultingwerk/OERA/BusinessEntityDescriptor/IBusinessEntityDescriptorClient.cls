/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IBusinessEntityDescriptorClient
    Purpose     : Serivce Interface for caching services for the Business
                  Entity Descriptor. Used by ABL Clients only
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 12 16:14:27 CEST 2015
    Notes       : Code executed on the backend is not supposed to use this
                  caching service, as the Business Entity itself uses a
                  cache (in the IBusinessEntityDescriptorService) to
                  retrieve the Business Entity Descriptor
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityDescriptorClient:

    /**
     * Purpose: Clears the Cache
     * Notes:
     */
    METHOD PUBLIC VOID ClearCache ().

    /**
     * Purpose: Returns the Business Entity Descriptor of a Business Entity
     * Notes:
     * @param pcBusinessEntityName The name of the Business Entity to return the Business Entity Descriptor for
     * @return The Business Entity Descriptor of the given Business Entity
     */
    METHOD PUBLIC IBusinessEntityDescriptor GetBusinessEntityDescriptor (pcBusinessEntityName AS CHARACTER).

END INTERFACE.
