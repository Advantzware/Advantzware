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
/// Summary description for Bin
/// </summary>
[System.ComponentModel.DataObject]

public class Binitem:AppServerConnect.AppServer
{
	public Binitem()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemJobDataSet SelectBin(string prmUser, string prmAction, string prmOrderNum, string prmItemNum)
    {

        dsItemJobDataSet dsItemJob = new dsItemJobDataSet();
        dsItemJob = null;
        AppServerConnect();
        aoObject.BinItem(prmUser, prmAction, prmOrderNum, prmItemNum, ref dsItemJob);
        AppServerDisconnect();

        return dsItemJob;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemJobUpDataSet SelectBinJobUp(string prmUser, string prmAction, string prmOrderNum, string prmItemNum)
    {

        dsItemJobUpDataSet dsItemJobUp = new dsItemJobUpDataSet();
        dsItemJobUp = null;
        AppServerConnect();
        aoObject.BinItemUp(prmUser, prmAction, prmOrderNum, prmItemNum, ref dsItemJobUp);
        AppServerDisconnect();

        return dsItemJobUp;
    }
}
