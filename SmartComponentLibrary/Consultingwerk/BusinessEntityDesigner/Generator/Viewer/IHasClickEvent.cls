/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IHasClickEvent
    Purpose     : Interface from Control Generators that provide a click event
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 16 22:01:48 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH . 
USING Progress.Lang.*                                          FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.Viewer.IHasClickEvent:  
  
    /*------------------------------------------------------------------------------
        Purpose: Code block for the Click (CHOOSE) event
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ClickTriggerBlock AS LONGCHAR NO-UNDO 
	GET.
	SET. 

END INTERFACE.
