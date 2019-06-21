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
/// Summary description for Colors
/// </summary>
[System.ComponentModel.DataObject]

public class Colors:AppServerConnect.AppServer
{
	public Colors()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsColorDataSet SelectColor(string prmUser, string prmAction, string prmOrderNum ,string prmItemNum)
    {

        dsColorDataSet dsColor = new dsColorDataSet();
        dsColor = null;
        AppServerConnect();
        aoObject.ItemColor(prmUser, prmAction, prmOrderNum, prmItemNum, ref dsColor);
        AppServerDisconnect();

        return dsColor;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsColSelDataSet SelectColSel(string prmUser, string prmAction,string prmItemNum, string prmRawNum)
    {

        dsColSelDataSet dsColSel = new dsColSelDataSet();
        dsColSel = null;
        AppServerConnect();
        aoObject.ColorSel(prmUser, prmAction ,prmItemNum, prmRawNum, ref dsColSel);
        AppServerDisconnect();

        return dsColSel;
    }

}
