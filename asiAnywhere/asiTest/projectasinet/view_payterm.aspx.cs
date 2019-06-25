
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class view_payterm : System.Web.UI.Page
{
    
    protected void Page_Load(object sender, System.EventArgs e)
    {        
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "payterm_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        


        

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["customer_user_id"] = UserLogin.UserName;

            }

        }
        if (Convert.ToString(Session["payterms_list_add_new_payterms"]) == "addpayterms")
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            Session["payterms_list_add_new_payterms"] = null;
        }
        
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox terms = (TextBox)FormView1.FindControl("tcodeTextBox");
            terms.Focus();
            
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            
            TextBox dscr = (TextBox)FormView1.FindControl("dscrTextBox");
            dscr.Focus();

            TextBox chk = (TextBox)FormView1.FindControl("codTextBox");
            CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
            try
            {
                if (chk.Text == "1")
                    edi.Checked = true;
                else
                    edi.Checked = false;
                               
            }
            catch { }

           
        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            
            Label chk = (Label)FormView1.FindControl("codLabel");
            CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
            try
            {
                if (chk.Text == "1")
                    edi.Checked = true;
                else
                    edi.Checked = false;
            }
            catch { }
            
            try
            {
                Label reckey = (Label)FormView1.FindControl("vreckeyLabel");
                if(reckey.Text != "")
                Session["payterms_list_vend_reckey"] = reckey.Text.Trim();
                               
            }
            catch { }
           
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";

        }


    }
    protected void formview1_onunload(object sender, EventArgs e)
    {
        
        try
        {
            //Label reckey = (Label)FormView1.FindControl("vreckeyLabel");
            //Session["payterms_list_vend_reckey"] = reckey.Text.Trim();
        }
        catch { }
       
    }
    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
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


   
    
    protected void lnk_Listvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("payterm_list.aspx");
    }
    protected void lnk_viewvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_payterm.aspx");
    }

    protected void addButton_Click(object sender, EventArgs e)
    {
        string vactive = "";

        TextBox terms = (TextBox)FormView1.FindControl("tcodeTextBox");
        TextBox dscr = (TextBox)FormView1.FindControl("dscrTextBox");
        TextBox disrate = (TextBox)FormView1.FindControl("disc_rateTextBox");
        TextBox disday = (TextBox)FormView1.FindControl("disc_dysTextBox");
        TextBox net = (TextBox)FormView1.FindControl("net_dysTextBox");
        TextBox cutoff = (TextBox)FormView1.FindControl("CUT_dateTextBox");
        TextBox type = (TextBox)FormView1.FindControl("vtypeTextBox");
        TextBox code = (TextBox)FormView1.FindControl("codTextBox");
        CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox1");
                
        if (chk.Checked == true)
            vactive = "1";
        else
            vactive = "0";
        
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmtcode"].DefaultValue = terms.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr"].DefaultValue = dscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdisc_rate"].DefaultValue = disrate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdisc_dys"].DefaultValue = disday.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnet_dys"].DefaultValue = net.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCUT_date"].DefaultValue = cutoff.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvtype"].DefaultValue = type.Text.Trim();

        ObjectDataSource1.SelectParameters["prmcod"].DefaultValue = vactive;
       
        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        string vactive = "";

        TextBox terms = (TextBox)FormView1.FindControl("tcodeTextBox");
        TextBox dscr = (TextBox)FormView1.FindControl("dscrTextBox");
        TextBox disrate = (TextBox)FormView1.FindControl("disc_rateTextBox");
        TextBox disday = (TextBox)FormView1.FindControl("disc_dysTextBox");
        TextBox net = (TextBox)FormView1.FindControl("net_dysTextBox");
        TextBox cutoff = (TextBox)FormView1.FindControl("CUT_dateTextBox");
        TextBox type = (TextBox)FormView1.FindControl("vtypeTextBox");
        TextBox code = (TextBox)FormView1.FindControl("codTextBox");
        CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox1");

        if (chk.Checked == true)
            vactive = "1";
        else
            vactive = "0";

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmtcode"].DefaultValue = terms.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr"].DefaultValue = dscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdisc_rate"].DefaultValue = disrate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdisc_dys"].DefaultValue = disday.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnet_dys"].DefaultValue = net.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCUT_date"].DefaultValue = cutoff.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvtype"].DefaultValue = type.Text.Trim();

        ObjectDataSource1.SelectParameters["prmcod"].DefaultValue = vactive;

        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }


    protected void Deletebutton_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        //Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");
    }
    
    
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
    }
    



}
