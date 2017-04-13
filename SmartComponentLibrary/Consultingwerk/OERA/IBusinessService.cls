/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IBusinessService
    Purpose     : Non functional Interface for Business Services (functional
                  requirements; no methods required to implement IBusinessService)
    Syntax      :
    Description : Classes implementing this Interface may be registered in
                  the ServiceManager and can thus be reached as Business
                  Entities or Business Tasks via Service Adapter/Service
                  Interface
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 24 22:16:47 CET 2010
    Notes       : Implemented by the Consultingwerk.OERA.BusinessEntity
                  class
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.IBusinessService:

END INTERFACE.
