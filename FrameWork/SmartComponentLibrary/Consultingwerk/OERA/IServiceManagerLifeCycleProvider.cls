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
    File        : IServiceManagerLifeCycleProvider
    Purpose     : Interface for services that provide service life cycle
                  definition to the ServiceManagerImpl class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 28 09:58:01 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.IServiceManagerLifeCycleProvider:

    {Consultingwerk/OERA/ttServiceLifeCycleDefinition.i}

    /**
     * Purpose: Returns the temp-table with Service Life Cycle definitions to the caller
     * Notes:   From Consultingwerk/OERA/ttServiceLifeCycleDefinition.i
     * @param ttServiceLifeCycleDefinition OUTPUT Temp-Table
     */
    METHOD PUBLIC VOID GetDefinitions (OUTPUT TABLE ttServiceLifeCycleDefinition).

END INTERFACE.
