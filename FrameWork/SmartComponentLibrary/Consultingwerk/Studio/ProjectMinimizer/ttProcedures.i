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
    File        : ttProcedures
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 03 08:39:02 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttProcedures NO-UNDO {&REFERENCE-ONLY}  
    FIELD RunName       AS CHARACTER 
    FIELD FullPathName  AS CHARACTER 
    FIELD IsClass       AS LOGICAL INIT FALSE
    FIELD Found         AS LOGICAL INIT FALSE 
    
    INDEX RunName IS UNIQUE PRIMARY RunName 
    . 

