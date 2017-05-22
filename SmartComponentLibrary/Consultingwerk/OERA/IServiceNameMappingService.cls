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
    File        : IServiceNameMappingService
    Purpose     : Interface for services that support the mapping of
                  the business service name (e.g. Business Entity or
                  Business Task) to the actual class name
    Syntax      :
    Description : Supports mapping of short names to actual class names
                  (SCL-1171)
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 04 22:27:14 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IServiceNameMappingService:

    /**
     * Purpose: Returns the actual business service name for the providede class
     *          name
     * Notes:
     * @param pcServiceName The service name as provided by the consumer
     * @return The name of the actual business service implementing class
     */
    METHOD PUBLIC CHARACTER GetBusinessServiceName (pcServiceName AS CHARACTER).

END INTERFACE.
