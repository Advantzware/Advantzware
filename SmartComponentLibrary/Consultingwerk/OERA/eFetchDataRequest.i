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
    File        : eFetchDataRequest.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 18 00:33:57 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eFetchDataRequest NO-UNDO {&REFERENCE-ONLY} 
    FIELD Context AS CHARACTER  
    FIELD CustomContext AS CHARACTER 
    FIELD Joins AS CHARACTER
    FIELD NextContext AS CHARACTER  
    FIELD NumRecords AS INTEGER  
    FIELD Positions AS CHARACTER  
    FIELD PrevContext AS CHARACTER  
    FIELD Queries AS CHARACTER  
    FIELD Requests AS CHARACTER  
    FIELD StopAfter AS INTEGER
    FIELD Tables AS CHARACTER  

    INDEX idx IS UNIQUE Tables .
     
    