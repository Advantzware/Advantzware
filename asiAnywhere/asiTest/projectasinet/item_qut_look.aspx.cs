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

public partial class item_qut_look : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["PrmUser"].DefaultValue = UserLogin.UserName;

        if (!Page.IsPostBack)
        {
            //ddlSearchOperation.SelectedIndex = 1;
            
        }

    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue;
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = "EQUAL";
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();
        
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "sea";
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
       
       //Response.Write(Session["genestno"]);
        //GridView1.Visible = false;
        //GridView2.Visible = true;
        //tblSearch.Visible = false;
        
        
    }
}
