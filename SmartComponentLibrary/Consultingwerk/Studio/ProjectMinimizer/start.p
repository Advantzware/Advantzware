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
    File        : start.p
    Purpose     : Starts the ProjectMinimizerForm

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingerk Ltd.
    Created     : Sun Jul 03 08:30:16 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*               FROM PROPATH .
USING Consultingwerk.Studio.ProjectMinimizer.* FROM PROPATH .

DEFINE VARIABLE oForm AS ProjectMinimizerForm NO-UNDO . 

/* ***************************  Main Block  *************************** */

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE .

oForm = NEW ProjectMinimizerForm () .

WAIT-FOR System.Windows.Forms.Application:Run (oForm) .
&ENDIF
