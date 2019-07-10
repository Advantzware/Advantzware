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

public partial class replacejobmt: System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {       
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            GridView1.SelectedIndex = 0;
        }
        catch { }
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
        FormView1.ChangeMode(FormViewMode.Edit);
        if (!Page.IsPostBack)
        {            
        }
        
        if (GridView1.Rows.Count.ToString() == "1" || GridView1.Rows.Count.ToString() == "0")
            griddiv.Visible = false;
        else
            formviewdiv.Visible = false;

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        TextBox out2 = (TextBox)FormView1.FindControl("voutTextBox");
        Label reckey = (Label)FormView1.FindControl("rec_keyLabel");
        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "submit";
        ObjectDataSource1.SelectParameters["prmvout"].DefaultValue = out2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();
        if (Convert.ToString(Request.QueryString["mode"]) == "1")
            Response.Write("<script>javascript:top.opener.window.caljob(1);javascript:window.close();</script>");
        if (Convert.ToString(Request.QueryString["mode"]) == "2")
            Response.Write("<script>javascript:top.opener.window.caljob(2);javascript:window.close();</script>");
    }
    protected void okButton_Click(object sender, EventArgs e)
    {
        griddiv.Visible = false;
        formviewdiv.Visible = true;


    }

   
}
