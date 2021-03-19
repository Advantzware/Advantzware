&Scoped-define TABLENAME customerindustry

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{custom/globdefs.i}

ASSIGN 
    {&TABLENAME}.customerIndustryID = NEXT-VALUE(customerIndustryID_seq)
    {&TABLENAME}.company = g_company.