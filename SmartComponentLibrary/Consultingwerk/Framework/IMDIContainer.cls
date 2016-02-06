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
    File        : IMDIContainer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 07 10:35:56 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

INTERFACE Consultingwerk.Framework.IMDIContainer:  
  
    METHOD PUBLIC VOID ActivateWindowTitleEventHandler ().

     METHOD PUBLIC VOID DeactivateWindowTitleEventHandler ().

    METHOD PUBLIC VOID MdiChildActivateHandler ().

 END INTERFACE.