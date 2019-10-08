/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : review.p
    Purpose     : Review Daemon Script 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Jun 20 10:21:46 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW . 

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Studio.P4.*             FROM PROPATH .  

DEFINE VARIABLE oReview      AS Review        NO-UNDO .
DEFINE VARIABLE oDescribe    AS Describe      NO-UNDO .  
DEFINE VARIABLE oChanges     AS CharacterList NO-UNDO . 

DEFINE VARIABLE cLast AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

oReview = NEW Review () .  
oDescribe = NEW Describe () . 

oChanges = oReview:GetReviewChangelists () .

{Consultingwerk/foreachPrimitiveList.i Character cChange IN oChanges}

    oDescribe:ToFile (cChange, SUBSTITUTE ("Changes/&1.txt":U, cChange)) .        

    ASSIGN cLast = cChange .

END.

OUTPUT TO last.change .
PUT UNFORMATTED cLast .
OUTPUT CLOSE . 

RETURN "0":U .
