/* aoaURL.i */

RUN aoa/aoaURL.p ("-AppService asAOA -H "
                + aoaHost
                + " -S " + STRING(aoaPort)
                , "Company|" + aoaCompany
                + "|UserID|" + aoaUserID
                + "|Name|"   + aoaName + "."
                , aoaURL
                  ).

