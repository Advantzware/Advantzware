/* spDynInitializeProc.p - rstark - 2.27.2019 */

/* add dynamic initialize procedures in alphabetical order */
/* always use a RETURN value of datatype character         */

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

/* **********************  Internal Functions  ************************ */

/* **********************  Internal Procedures  *********************** */

PROCEDURE dynInitCompany:
    RETURN cCompany.
END PROCEDURE.

PROCEDURE dynInitCompanyList:
    DEFINE VARIABLE cCompanyList AS CHARACTER NO-UNDO.
    
    FOR EACH company NO-LOCK:
        cCompanyList = cCompanyList + company.company + ",".
    END. /* each company */
    cCompanyList = TRIM(cCompanyList).
    
    RETURN cCompanyList.
END PROCEDURE.

PROCEDURE spSetCompany:
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    cCompany = ipcCompany.
END PROCEDURE.
