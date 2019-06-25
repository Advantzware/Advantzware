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

public partial class fold_bom : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        try
        {
            UserClass UserLogin = (UserClass)Session["User"];
            FormView1.ChangeMode(FormViewMode.Edit);
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        }
        catch
        {
            Response.Write("Session Expired! Please Relogin");
        }
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        try
        {
            TextBox medium = (TextBox)FormView1.FindControl("vMediumTextBox");
            RadioButtonList flute = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
            TextBox liner = (TextBox)FormView1.FindControl("vLinerTextBox");
            TextBox lamcode = (TextBox)FormView1.FindControl("vLamCodeTextBox");
            TextBox adhesive = (TextBox)FormView1.FindControl("vAdhesiveTextBox");


            UserClass UserLogin = (UserClass)Session["User"];

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmMedium"].DefaultValue = medium.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLiner"].DefaultValue = liner.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLamCode"].DefaultValue = lamcode.Text.Trim();
            ObjectDataSource1.SelectParameters["prmAdhesive"].DefaultValue = adhesive.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFlute"].DefaultValue = flute.SelectedValue.ToString();
            
            FormView1.ChangeMode(FormViewMode.Edit);
        }
        catch { }
    }
    protected void FromView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            try
            {
                TextBox flute = (TextBox)FormView1.FindControl("vFluteTextBox");
                RadioButtonList rdl = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
                if (flute.Text == "125")
                    rdl.SelectedIndex = 0;
                if (flute.Text == "78")
                    rdl.SelectedIndex = 1;
                if (flute.Text == "52")
                    rdl.SelectedIndex = 2;
                if (flute.Text == "0" || flute.Text == "")
                    rdl.SelectedIndex = 3;
            }
            catch { }
        }
    }
}
