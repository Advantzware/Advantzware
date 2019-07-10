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

public partial class set_update_est2 : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;  
    }

    protected void updatebutton_Click(object sender, EventArgs e)
    {
        TextBox stock = (TextBox)FormView1.FindControl("vStockTextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("vPartnoTextBox");
        TextBox itemname = (TextBox)FormView1.FindControl("vPartDscrTextBox");
        TextBox partdesc = (TextBox)FormView1.FindControl("vPartDscr2TextBox");
        TextBox cat = (TextBox)FormView1.FindControl("vProcatTextBox");
        TextBox len = (TextBox)FormView1.FindControl("vLenTextBox");
        TextBox wid = (TextBox)FormView1.FindControl("vWidTextBox");
        TextBox dep = (TextBox)FormView1.FindControl("vDepTextBox");
        RadioButtonList all = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        CheckBox unit = (CheckBox)FormView1.FindControl("CheckBox1");
        if (all.SelectedIndex == 0)
            HiddenField1.Value = "Yes";
        if (all.SelectedIndex == 1)
            HiddenField1.Value = "No";
        if (all.SelectedIndex == 2)
            HiddenField1.Value = "?";
        if (unit.Checked == true)
            HiddenField2.Value = "Yes";
        else HiddenField2.Value = "No";

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmStock"].DefaultValue = stock.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPartno"].DefaultValue = custpart.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPartDscr"].DefaultValue = itemname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPartDscr2"].DefaultValue = partdesc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmProcat"].DefaultValue = cat.Text.Trim();
        ObjectDataSource1.SelectParameters["prmlen"].DefaultValue = len.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWid"].DefaultValue = wid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDep"].DefaultValue = dep.Text.Trim();

        ObjectDataSource1.SelectParameters["prmAllo"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmUnit"].DefaultValue = HiddenField2.Value;


    }
    
    
}
