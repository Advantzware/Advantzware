/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : gene4gl.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Nov 01 14:33:20 GMT+01:00 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{src/web/method/cgidefs.i NEW}

DEFINE INPUT        PARAMETER cFileName  AS CHARACTER NO-UNDO .
DEFINE INPUT-OUTPUT PARAMETER pcOptions  AS CHARACTER NO-UNDO .
DEFINE INPUT-OUTPUT PARAMETER pcFileName AS CHARACTER NO-UNDO .

/* ***************************  Main Block  *************************** */

RUN webutil/e4gl-gen.p (cFileName,
                        INPUT-OUTPUT pcOptions,
                        INPUT-OUTPUT pcFileName) .
                        