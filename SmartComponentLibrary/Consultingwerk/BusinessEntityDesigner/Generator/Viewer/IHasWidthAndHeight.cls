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
    File        : IHasWidthAndHeight
    Purpose     : Interfacer for ControlGenerator implementations that have
                  a Width and Height property
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 10 15:55:34 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .  
USING Progress.Lang.*                                          FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.Viewer.IHasWidthAndHeight:  

    /*------------------------------------------------------------------------------
        Purpose: The Height of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Height AS INTEGER NO-UNDO 
    GET.
    SET. 
      
    /*------------------------------------------------------------------------------
        Purpose: The Width of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Width AS INTEGER NO-UNDO  
    GET.
    SET. 
      
END INTERFACE.
