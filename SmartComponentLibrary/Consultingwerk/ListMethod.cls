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
    File        : ListMethod
    Purpose     : Generic List of Methods (OpenEdge 11.6 reflection)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 16 11:57:14 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Consultingwerk.*                FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.ListMethod 
    INHERITS GenericList: 

&IF DEFINED (AblReflection) NE 0 &THEN

    {Consultingwerk/Framework/Base/GenericList.i Progress.Reflect.Method}

&ENDIF

END CLASS.
