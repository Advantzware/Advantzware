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
    File        : resizelib.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 22 18:20:56 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE NEW GLOBAL SHARED VARIABLE hResizeLib AS WIDGET-HANDLE NO-UNDO.       

/* ***************************  Main Block  *************************** */

IF NOT VALID-HANDLE(hResizeLib) THEN DO:
    RUN src/ResizeLib/ResizeLib.p PERSIST SET hResizeLib. 
    SESSION:ADD-SUPER-PROC(hResizeLib).         
END.
