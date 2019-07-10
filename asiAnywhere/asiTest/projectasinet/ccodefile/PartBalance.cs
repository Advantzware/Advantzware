using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET;
using ASIDataNS;
using Progress.Open4GL.Proxy;

/// <summary>
/// Summary description for ItemDetail
/// </summary>
[System.ComponentModel.DataObject]
public class PartBalance : AppServerConnect.AppServer
{
    public PartBalance()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
//    public dsPartBalanceDataSet SelectPartBalance(string prmItem, string vRefresh, string vStartdate, string vEnddate)
    public dsPartBalanceDataSet SelectPartBalance(string prmItem)
    {

        dsPartBalanceDataSet dsPartBalance = new dsPartBalanceDataSet();
        dsPartBalance = null;
        AppServerConnect();
        aoObject.PartBalance("001", "ATT1000", "", prmItem, "", "", "", ref dsPartBalance);
        AppServerDisconnect();

        return dsPartBalance;
    }
    
}


