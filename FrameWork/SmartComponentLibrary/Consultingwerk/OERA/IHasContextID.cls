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
    File        : IHasContextID
    Purpose     : Interface for Business Services (e.g. Business Entities) 
                  that support multiple instances in the ServiceManager 
                  (ServiceInterface) distinguished by a ContextID
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 01 16:48:51 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.IHasContextID:  

    /*------------------------------------------------------------------------------
        Purpose: A Context ID / Instance ID                                                                       
        Notes:   Used to distinguish multiple instances of the same Business Service
                 Class                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContextID AS CHARACTER NO-UNDO 
    GET.
    SET. 

END INTERFACE.