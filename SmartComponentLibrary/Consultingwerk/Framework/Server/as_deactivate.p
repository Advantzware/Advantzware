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
    File        : as_deactivate.p
    Purpose     : AppServer Deactivate Procedure

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Mar 29 21:06:48 CET 2016
    Notes       : Invokes the ServiceManager:ProcessServiceLifeCycle method
                  to clean up Business Service instances based on the life
                  cycle definition
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*         FROM PROPATH .
USING Consultingwerk.OERA.*              FROM PROPATH .
USING Consultingwerk.Util.*              FROM PROPATH .

/* ***************************  Main Block  *************************** */

/* SCL-1494: Now happening in ServiceManagerImpl */
/* ServiceManager:ProcessServiceLifeCycle () .*/
