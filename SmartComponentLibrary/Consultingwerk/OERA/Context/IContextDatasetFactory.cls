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
    File        : IContextDatasetFactory
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 19 12:49:03 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.Context.IContextDatasetFactory:  
  
    /*------------------------------------------------------------------------------
        Purpose: Factory method for the context dataset                                                                         
        Notes:   Returns the handle for the Context Dataset
        @return The handle of the context dataset instance                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE CreateContextDataset ().

END INTERFACE.
