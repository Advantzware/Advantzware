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

public partial class corr_vendor_cost : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (!Page.IsPostBack)
        {
            try
            {
                GridView1.SelectedIndex = 0;
                Session["corr_vendor_cost_item_get"] = GridView1.SelectedRow.Cells[3].Text;
                HiddenField1.Value = ((Label)GridView1.SelectedRow.FindControl("recidlabel")).Text;
            }
            catch { }           
        }

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {

    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["corr_vendor_cost_item_get"] = GridView1.SelectedRow.Cells[3].Text;
        HiddenField1.Value = ((Label)GridView1.SelectedRow.FindControl("recidlabel")).Text;
    }
}
