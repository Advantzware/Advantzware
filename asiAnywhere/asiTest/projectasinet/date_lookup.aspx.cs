using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class date_lookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {

    }
    protected void Calendar1_SelectionChanged(object sender, EventArgs e)
    {
        string date = Calendar1.SelectedDate.ToString("MM/dd/yyy");
        Response.Write("<script>window.opener.Datelookup('" + date + "');</script>");
        Response.Write("<script>window.close();</script>");

    }
}
