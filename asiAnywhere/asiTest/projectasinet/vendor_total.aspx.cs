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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class verdor_total : System.Web.UI.Page
{
    
        

    protected void Page_Load(object sender, EventArgs e)
    {
                
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];      

        
          
        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "vendor_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            lblUser.Text = UserLogin.UserName;
            labelcompany.Text = PrmComp;

            /*if (aUsers == "external")
            {
                customerid.Visible = false;

            }
            */

        }

    }

   
    
    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label vend = (Label)FormView1.FindControl("vendLabel");
        Label vendname = (Label)FormView1.FindControl("vendnameLabel");
        TextBox purch = (TextBox)FormView1.FindControl("purchTextBox");
        TextBox lstyr = (TextBox)FormView1.FindControl("lst_yrTextBox");
        TextBox ytdmsf = (TextBox)FormView1.FindControl("ytd_msfTextBox");
        TextBox lyytdmsf = (TextBox)FormView1.FindControl("lyytd_msfTextBox");
        TextBox hibal = (TextBox)FormView1.FindControl("hibalTextBox");
        TextBox hidate = (TextBox)FormView1.FindControl("hibal_dateTextBox");
        TextBox numinv = (TextBox)FormView1.FindControl("num_invTextBox");
        TextBox lpay = (TextBox)FormView1.FindControl("lpayTextBox");
        TextBox ldate = (TextBox)FormView1.FindControl("lpay_dateTextBox");
        TextBox avgpay = (TextBox)FormView1.FindControl("AVG_payTextBox");
        TextBox accbal = (TextBox)FormView1.FindControl("acc_balTextBox");
        TextBox puchase = (TextBox)FormView1.FindControl("purchaseTextBox");
        TextBox totmsf = (TextBox)FormView1.FindControl("tot_msfTextBox");
        TextBox ordbal = (TextBox)FormView1.FindControl("ordbalTextBox");
        TextBox reckey = (TextBox)FormView1.FindControl("reckeyTextBox");
        
        
        

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vend.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvendname"].DefaultValue = vendname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpurch"].DefaultValue = purch.Text.Trim();
        ObjectDataSource1.SelectParameters["prmlst_yr"].DefaultValue = lstyr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmytd_msf"].DefaultValue = ytdmsf.Text.Trim();
        ObjectDataSource1.SelectParameters["prmlyytd_msf"].DefaultValue = lyytdmsf.Text.Trim();
        ObjectDataSource1.SelectParameters["prmhibal"].DefaultValue = hibal.Text.Trim();
        ObjectDataSource1.SelectParameters["prmhibal_date"].DefaultValue = hidate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnum_inv"].DefaultValue = numinv.Text.Trim();
        ObjectDataSource1.SelectParameters["prmlpay"].DefaultValue = lpay.Text.Trim();
        ObjectDataSource1.SelectParameters["prmlpay_date"].DefaultValue = ldate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAVG_pay"].DefaultValue = avgpay.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacc_bal"].DefaultValue = accbal.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpurchase"].DefaultValue = puchase.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtot_msf"].DefaultValue = totmsf.Text.Trim();
        ObjectDataSource1.SelectParameters["prmordbal"].DefaultValue = ordbal.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = reckey.Text.Trim();
        
        FormView1.ChangeMode(FormViewMode.ReadOnly);
                
    }


    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            TextBox reckey = (TextBox)FormView1.FindControl("reckeyTextBox");
            Session["vendor_totl"] = reckey.Text;

        }
        catch { }
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void lnk_viewvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("vend_viewlist.aspx");
    }

    protected void lnk_listview_Click(object sender, EventArgs e)
    {
        Response.Redirect("vendor_total.aspx");
    }
    protected void Ink_vendorlist(object sender, EventArgs e)
    {
        Response.Redirect("vendor_list.aspx");
    }
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        Session["vendor_list_add_new_vender"] = "addvendor";
        Response.Redirect("vend_viewlist.aspx");
    }
    
 

 
 
    
}
