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
    File        : todo-annotation-parser.p
    Purpose     : Parses all source code in the workspace for @TODO and 
                  @TO-DO annotations and reports the result in a JUnit
                  compatible output file 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 17 13:56:18 CEST 2014
    Notes       : "comment" attribute of the annotation is reported to the 
                  output file 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.SmartUnit.*             FROM PROPATH . 
USING Consultingwerk.Studio.*                FROM PROPATH . 
USING Consultingwerk.Util.*                  FROM PROPATH . 

{Consultingwerk/products.i}
{Consultingwerk/Util/TempTables/ttFileNames.i}

{Consultingwerk/SmartUnit/dsTestsuites.i}

DEFINE VARIABLE oParser      AS AnnotationParser NO-UNDO . 
DEFINE VARIABLE oAnnotations AS ListAnnotation   NO-UNDO . 
DEFINE VARIABLE cClassName   AS CHARACTER        NO-UNDO .

/* ***************************  Main Block  *************************** */

CREATE eTestSuite . 
ASSIGN eTestSuite.TestsuiteName = "TO-DO Annotations":U .

FileHelper:GetFileList (".":U,
                        "*.p,*.w,*.cls":U,
                        OUTPUT TABLE ttFileNames BY-REFERENCE) .

filenames:
FOR EACH ttFileNames ON ERROR UNDO, THROW:

    ASSIGN cClassName = SUBSTRING (ttFileNames.FileName,
                                   1,
                                   R-INDEX (ttFileNames.FileName, ".":U) - 1) 
           cClassName = REPLACE (cClassName, "~\":U, ".":U)
           cClassName = REPLACE (cClassName, "/":U, ".":U)
           .

    IF cClassName BEGINS ".~\":U OR cClassName BEGINS "./":U THEN 
        cClassName = SUBSTRING (cClassName, 3) .

    CREATE eTestCase . 
    ASSIGN eTestCase.TestsuiteName = eTestSuite.TestSuiteName
           eTestCase.TestcaseName  = "todo":U 
           eTestCase.ClassName     = cClassName .

    DO ON ERROR UNDO, THROW:
        oParser      = NEW AnnotationParser (ttFileNames.FileName) .
        
        CATCH err AS Progress.Lang.Error:
        	MESSAGE err:GetMessage (1)
                VIEW-AS ALERT-BOX.	
            NEXT filenames.
        END CATCH.
    END. 
       
    oAnnotations = oParser:FindAnnotations ("TODO":U) .
    
    IF oAnnotations:Count > 0 THEN DO:
        
        IF oAnnotations:GetItem (1):Parameters:ContainsName ("comment":U) THEN 
            eTestCase.ErrorMessage = oAnnotations:GetItem (1):Parameters:FindByName ("comment":U):Value .
        ELSE  
            eTestCase.ErrorMessage = "Generic TO-DO":U . 

        NEXT filenames .
    END.

    oAnnotations = oParser:FindAnnotations ("TO-DO":U) .
    
    IF oAnnotations:Count > 0 THEN DO:
        
        IF oAnnotations:GetItem (1):Parameters:ContainsName ("comment":U) THEN 
            eTestCase.ErrorMessage = oAnnotations:GetItem (1):Parameters:FindByName ("comment":U):Value .
        ELSE  
            eTestCase.ErrorMessage = "Generic TO-DO":U . 

        NEXT filenames .
    END.

END.
    
RETURN "0":U . /* PCTRun requirement */

CATCH ex AS System.Exception:
    DEFINE VARIABLE oError AS Consultingwerk.Exceptions.Exception NO-UNDO . 
    
    MESSAGE SUBSTITUTE ("Error validating: &1 (&2)":U, 
                        ttFileNames.FileName,
                        cClassName) .
    
    oError = NEW Consultingwerk.Exceptions.Exception (ex,
                                                      SUBSTITUTE ("Error validating: &1 ":U, 
                                                                  ttFileNames.FileName),
                                                      0) .

    IF ex:NumMessages > 0 THEN DO:
        MESSAGE err:GetMessage (1) .
                
        oError:AddMessage (ex:GetMessage (1),
                           ex:GetMessageNum (1)) .                                                      
    END.                                   
                                                        
    UNDO, THROW oError .
END CATCH.
                        
FINALLY:
    SmartUnit:WriteTestResult ("output/todo.xml":U,     
                               DATASET dsTestsuites) .      
END FINALLY.

                        