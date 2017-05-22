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
    File        : IBusinessEntityDescriptorService
    Purpose     : Interface for services that can provide the Business Entity
                  Descriptor for a Business Entity
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 10 18:04:10 CEST 2015
    Notes       : This Service is used by the Business Entities to return
                  the Busines Entity Descriptor. This service should not
                  be called from consumers
                  http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

INTERFACE Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityDescriptorService:

    /**
     * Purpose: Clears the cache
     * Notes:
     */
    METHOD PUBLIC VOID ClearCache () .

    /**
     * Purpose: Returns the Business Entity Descriptor of a Business Entity
     * Notes:
     * @param pcBusinessEntityName The name of the Business Entity to return the Business Entity Descriptor for
     * @return The Business Entity Descriptor of the given Business Entity
     */
    METHOD PUBLIC IBusinessEntityDescriptor GetBusinessEntityDescriptor (pcBusinessEntityName AS CHARACTER).

END INTERFACE.
