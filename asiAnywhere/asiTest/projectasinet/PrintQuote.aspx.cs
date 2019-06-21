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

public partial class printquote : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        showlabel.Visible = false;
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        try
        {
            Label print = (Label)FormView1.FindControl("aFileLabel");


            if (print.Text != "")
            {
                string path = print.Text;

                string path2 = @"/pdfs/" + path;
                //Response.Write(path2);
                Response.Redirect(path2);
            }
            else
            {
                showlabel.Text = "No PDF Exists";
                showlabel.Visible = true;
            }
        }
        catch
        {
            showlabel.Text = "No PDF Exists";
            showlabel.Visible = true;
        }

    }
}