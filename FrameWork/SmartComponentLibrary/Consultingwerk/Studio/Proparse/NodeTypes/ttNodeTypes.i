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
    File        : ttNodeTypes.i
    Purpose     : Temp-Table of nodetypes with NodeType name, ABL keyword 
                  and min arrreviated chars  

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Jul 22 11:39:08 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttNodeTypes NO-UNDO 
    FIELD NodeType       AS CHARACTER 
    FIELD AblKeyWord     AS CHARACTER 
    FIELD MinAbbreviated AS INTEGER 
    
    INDEX NodeType IS UNIQUE PRIMARY NodeType
    INDEX AblKeyWord IS UNIQUE AblKeyWord . 
