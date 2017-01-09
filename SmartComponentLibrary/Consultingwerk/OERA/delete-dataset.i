/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : delete-dataset.i
    Purpose     : Deletes a DATASET-HANDLE Parameter, when not passed 
                  with BY-REFERENCE

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Sep 01 14:22:52 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

&IF "{2}" NE "" &THEN
&SCOPED-DEFINE errorvar {2}
&ELSE
&SCOPED-DEFINE errorvar err
&ENDIF

        IF VALID-HANDLE ({1}) AND {1}:NUM-REFERENCES = 0 THEN DO ON ERROR UNDO, THROW:                                 
            DELETE OBJECT {1} .
    
            /* Handle Cannot delete a BY-REFERENCE PARAMETER dataset or table in the called procedure. (12327) */
            CATCH {&errorvar} AS Progress.Lang.Error :
                IF {&errorvar}:GetMessageNum (1) <> 12327 THEN  
                    UNDO, THROW {&errorvar} . 
            END CATCH.
        END .
