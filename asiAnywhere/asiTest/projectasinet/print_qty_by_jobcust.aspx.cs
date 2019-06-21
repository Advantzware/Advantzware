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

public partial class print_qty_by_jobcust : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["qty_by_jobcust"] != null)
        {
            string open = Convert.ToString(Session["qty_by_jobcust"]);
            Response.Redirect(open);
        }
    }
}
