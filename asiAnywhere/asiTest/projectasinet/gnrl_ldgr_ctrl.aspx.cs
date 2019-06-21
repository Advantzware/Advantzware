
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class gnrl_ldgr_ctrl : System.Web.UI.Page
{
        
    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "gnrl_ldgr_ctrl.aspx";
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
       

    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox jrnl = (TextBox)FormView1.FindControl("lstjrnl_TextBox");

            jrnl.Focus();


        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {

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




    

    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        TextBox jrnl = (TextBox)FormView1.FindControl("lstjrnl_TextBox");
        TextBox trns = (TextBox)FormView1.FindControl("trnsTextBox");
        TextBox curyr = (TextBox)FormView1.FindControl("curryr_TextBox");
        Label curdscr = (Label)FormView1.FindControl("curdscr_TextBox");
        TextBox prof = (TextBox)FormView1.FindControl("proctr_TextBox");
        Label prodscr = (Label)FormView1.FindControl("prodscr_TextBox");


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmjrnl"].DefaultValue = jrnl.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtrns"].DefaultValue = trns.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcrtyr"].DefaultValue = curyr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcrtdscr"].DefaultValue = curdscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmprofit"].DefaultValue = prof.Text.Trim();
        ObjectDataSource1.SelectParameters["prmprodscr"].DefaultValue = prodscr.Text.Trim();

        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }



   
    protected void Formview_Unload(object sender, EventArgs e)
    {

    }




}
