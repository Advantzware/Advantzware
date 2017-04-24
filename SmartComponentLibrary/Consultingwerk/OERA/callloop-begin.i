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
    File        : callloop-begin.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 22 14:53:17 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

        &IF DEFINED (DotNetAccessible) NE 0 &THEN
            callloop:
            REPEAT ON ERROR UNDO, THROW:
        &ENDIF
