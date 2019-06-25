
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class view_currency : System.Web.UI.Page
{


    protected void Page_Load(object sender, System.EventArgs e)
    {

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_gl_bank.aspx";
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
            if (Convert.ToString(Session["currency_list_add_button"]) == "add")
            {
                FormView1.ChangeMode(FormViewMode.Insert);
                Session["currency_list_add_button"] = null;
            }

            
        }
        
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


    

    protected void addButton_Click(object sender, EventArgs e)
    {

        TextBox currcode = (TextBox)FormView1.FindControl("c_codeTextBox");
        TextBox desc = (TextBox)FormView1.FindControl("c_descTextBox");
        TextBox country = (TextBox)FormView1.FindControl("countryTextBox");
        TextBox num = (TextBox)FormView1.FindControl("c_numTextBox");
        TextBox minor = (TextBox)FormView1.FindControl("minor_unitTextBox");
        CheckBox vbase = (CheckBox)FormView1.FindControl("CheckBox1");
        TextBox exrate = (TextBox)FormView1.FindControl("ex_rateTextBox");
        TextBox account = (TextBox)FormView1.FindControl("ar_ast_acctTextBox");


        UserClass UserLogin = (UserClass)Session["User"];


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "AddNewRec";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmCcode"].DefaultValue = currcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCdesc"].DefaultValue = desc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCountry"].DefaultValue = country.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCnum"].DefaultValue = num.Text.Trim();
        ObjectDataSource1.SelectParameters["prMinorunit"].DefaultValue = minor.Text.Trim();
        ObjectDataSource1.SelectParameters["prmIsbase"].DefaultValue = Convert.ToString(vbase.Checked);
        ObjectDataSource1.SelectParameters["prmExrate"].DefaultValue = exrate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmArastacct"].DefaultValue = account.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }

    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        TextBox currcode = (TextBox)FormView1.FindControl("c_codeTextBox");
        TextBox desc = (TextBox)FormView1.FindControl("c_descTextBox");
        TextBox country = (TextBox)FormView1.FindControl("countryTextBox");
        TextBox num = (TextBox)FormView1.FindControl("c_numTextBox");
        TextBox minor = (TextBox)FormView1.FindControl("minor_unitTextBox");
        CheckBox vbase = (CheckBox)FormView1.FindControl("CheckBox1");
        TextBox exrate = (TextBox)FormView1.FindControl("ex_rateTextBox");
        TextBox account = (TextBox)FormView1.FindControl("ar_ast_acctTextBox");
        

        UserClass UserLogin = (UserClass)Session["User"];


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmCcode"].DefaultValue = currcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCdesc"].DefaultValue = desc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCountry"].DefaultValue = country.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCnum"].DefaultValue = num.Text.Trim();
        ObjectDataSource1.SelectParameters["prMinorunit"].DefaultValue = minor.Text.Trim();
        ObjectDataSource1.SelectParameters["prmIsbase"].DefaultValue = Convert.ToString(vbase.Checked);
        ObjectDataSource1.SelectParameters["prmExrate"].DefaultValue = exrate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmArastacct"].DefaultValue = account.Text.Trim();
        //FormView1.ChangeMode(FormViewMode.ReadOnly);

        Response.Write("<script> window.location.href='view_currency.aspx' </script>");
    }


    protected void delete_Button_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DeleteRec";
        Session["customer1_list_index"] = null;
        /* Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");*/
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }

    protected void lnk_listcurrency(object sender, EventArgs e)
    {
        Response.Redirect("currency_list.aspx");
    }
    protected void lnk_viewcurr_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_currency.aspx");
    }
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);  
    }
    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
    protected void FormView1_ondataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox bou = (TextBox)FormView1.FindControl("is_baseTextBox");
            CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox1");
            TextBox currcode = (TextBox)FormView1.FindControl("c_codeTextBox");
            currcode.Focus();
            if (bou.Text == "Yes" || bou.Text == "yes")
                chk.Checked = true;
            else
                chk.Checked = false;

        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox currcode = (TextBox)FormView1.FindControl("c_codeTextBox");
            currcode.Focus();
            TextBox bou = (TextBox)FormView1.FindControl("is_baseTextBox");
            CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox1");
            TextBox num = (TextBox)FormView1.FindControl("c_numTextBox");
            TextBox exrate = (TextBox)FormView1.FindControl("ex_rateTextBox");
            if (bou.Text == "Yes" || bou.Text == "yes")
                chk.Checked = true;
            else
                chk.Checked = false;
            num.Text = "0";
            exrate.Text = "0.00";

        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            //try
            //{
            //    Label reckey = (Label)FormView1.FindControl("reckeyLabel");
            //    Session["currency_list_reckey"] = reckey.Text.Trim();
            //}
            //catch { }

        }
    }
    



}