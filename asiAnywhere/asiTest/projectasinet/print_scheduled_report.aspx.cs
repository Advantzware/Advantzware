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

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class scheduled_list : System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {
        if (Session["scheduled_rel_list"] != null)
        {
            string open = Convert.ToString(Session["scheduled_rel_list"]);
            Response.Redirect(open);
        }
    }
}
