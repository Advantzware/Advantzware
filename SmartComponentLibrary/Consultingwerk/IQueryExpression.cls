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
    File        : IQueryExpression
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : Wed Oct 10 10:24:17 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH.

INTERFACE Consultingwerk.IQueryExpression:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the Expression to be used in a Query String
        Notes:   
        @return The CHARACTER Expression  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetExpression ().

END INTERFACE.